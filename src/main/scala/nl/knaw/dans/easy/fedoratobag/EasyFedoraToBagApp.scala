/*
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.fedoratobag

import better.files.File.CopyOptions
import better.files.{ File, StringExtensions }
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import com.yourmediashelf.fedora.client.{ FedoraClient, FedoraClientException }
import nl.knaw.dans.bag.ChecksumAlgorithm
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedoratobag.Command.FeedBackMessage
import nl.knaw.dans.easy.fedoratobag.FileInfo.checkDuplicates
import nl.knaw.dans.easy.fedoratobag.FileItem.{ checkNotImplementedFileMetadata, filesXml }
import nl.knaw.dans.easy.fedoratobag.FoXml._
import nl.knaw.dans.easy.fedoratobag.OutputFormat.OutputFormat
import nl.knaw.dans.easy.fedoratobag.TransformationType._
import nl.knaw.dans.easy.fedoratobag.filter._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.apache.commons.csv.CSVPrinter
import org.joda.time.DateTime
import sun.net.util.URLUtil

import java.io.{ ByteArrayInputStream, IOException, InputStream }
import java.net.URL
import java.nio.file.Paths
import java.util.UUID
import javax.naming.ldap.InitialLdapContext
import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node, PCData, Text }

class EasyFedoraToBagApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val fsRdb = new FsRdb(configuration.databaseConnection)
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  lazy val bagIndex: BagIndex = filter.BagIndex(configuration.bagIndexUrl)
  private lazy val ldap = new Ldap(ldapContext)
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  private def logSkipped(datasetId: DatasetId, printer: CSVPrinter) = {
    new CsvRecord(datasetId, None, None, "", "", "","ignored").print(printer)
  }

  def createExport(input: List[InputFileRecord], skip: Seq[String], outputDir: File, options: Options, outputFormat: OutputFormat)
                  (printer: CSVPrinter): Try[FeedBackMessage] = {
    logger.info(options.toString)
    input.map { case InputFileRecord(datasetId: DatasetId, optUuid1, optUuid2) =>
      if (optUuid1.isEmpty && optUuid2.isDefined) throw new IllegalArgumentException("uuid1 may not be empty if uuid2 is provided")
      if (skip.contains(datasetId))
        Success(logSkipped(datasetId, printer))
      else {
        def bagDir(packageDir: File) = outputFormat match {
          case OutputFormat.AIP => packageDir
          case OutputFormat.SIP => packageDir / UUID.randomUUID.toString
        }

        val packageUuid1 = optUuid1.getOrElse(UUID.randomUUID)
        val packageUuid2 = optUuid2.getOrElse(UUID.randomUUID)
        val packageDir1 = configuration.stagingDir / packageUuid1.toString
        val packageDir2 = configuration.stagingDir / packageUuid2.toString
        val bagDir1 = bagDir(packageDir1)
        val bagDir2 = bagDir(packageDir2)
        def createSecondBag(datasetInfo: DatasetInfo) = {
          if (datasetInfo.nextBagFileInfos.isEmpty)  {
            if (optUuid1.isDefined && optUuid2.isDefined) Failure(new IllegalArgumentException("Input contained two version UUIDs, but generating only one version"))
            else Success(None)
          }
          else for {
            bag2 <- DansV0Bag.empty(bagDir2)
            _ = logger.info (s"exporting $datasetId to second bag $bagDir2")
            _ = bag2.withEasyUserAccount(datasetInfo.depositor).withCreated(DateTime.now())
            _ = BagVersion(datasetInfo.doi, datasetInfo.urn, packageUuid1)
              .addTo(bag2, Some(2))
            _ <- fillSecondBag(bag2, bagDir1 / "metadata", datasetInfo.nextBagFileInfos.toList)
            _ <- movePackageAtomically(packageDir2, outputDir)
          } yield Some(packageUuid2)
        }

        logger.info (s"exporting $datasetId to $bagDir1")
        val triedCsvRecord = for {
          datasetInfo <- createBag(datasetId, bagDir1, options)
          maybeUuid2 <- createSecondBag(datasetInfo)
          // first bag moved after second, thus a next process can stumble over a missing first bag in case of interrupts
          _ <- movePackageAtomically(packageDir1, outputDir)
          _ <- CsvRecord(datasetId, datasetInfo, packageUuid1, maybeUuid2, options).print(printer)
        } yield ()
        errorHandling(triedCsvRecord, printer, datasetId, packageUuid1)
      }
    }.failFastOr(Success("no fedora/IO errors"))
  }


  private def movePackageAtomically(packageDir: File, outputDir: File) = {
    val target = outputDir / packageDir.name
    debug(s"Moving $packageDir to output dir: $target")
    Try(packageDir.moveTo(target)(CopyOptions.atomically))
  }

  private def errorHandling[T](tried: Try[T], printer: CSVPrinter, datasetId: DatasetId, packageUUID: UUID) = {
    tried.doIfFailure {
      case t: InvalidTransformationException => logger.warn(s"$datasetId -> $packageUUID failed: ${ t.getMessage }")
      case t: Throwable => logger.error(s"$datasetId -> $packageUUID had a not expected exception: ${ t.getMessage }", t)
    }.recoverWith {
      case t: FedoraClientException if t.getStatus != 404 => Failure(t)
      case t: IOException => Failure(t)
      case t => CsvRecord(datasetId, Option(packageUUID), None, doi = "", depositor = "", "-", s"FAILED: $t")
        .print(printer)
        Success(())
    }
  }

  private def fillSecondBag(bag2: DansV0Bag, metadataOfBag1: File, fileInfos: List[FileInfo]) = {

    def copy(fileName: String) = (metadataOfBag1 / fileName)
      .inputStream
      .map(addMetadataStreamTo(bag2, fileName))
      .get

    for {
      _ <- copy("emd.xml")
      _ <- copy("amd.xml")
      _ <- copy("dataset.xml")
      _ <- metadataOfBag1.list.toList
        .filter(_.name.toLowerCase.contains("license"))
        .traverse(file => copy(file.name))
      _ <- copy("depositor-info/agreements.xml")
      fileItems <- fileInfos
        .traverse(addPayloadFileTo(bag2, isOriginalVersioned = true))
      _ <- checkNotImplementedFileMetadata(fileItems, logger)
      _ <- addXmlMetadataTo(bag2, "files.xml")(filesXml(fileItems))
      _ <- bag2.save
    } yield ()
  }

  def createBag(datasetId: DatasetId, bagDir: File, options: Options, maybeFirstBagVersion: Option[BagVersion] = None): Try[DatasetInfo] = {

    def managedMetadataStream(foXml: Elem, streamId: String, bag: DansV0Bag, metadataFile: String) = {
      managedStreamLabel(foXml, streamId)
        .map { label =>
          val extension = label.split("[.]").last
          val bagFile = s"$metadataFile.$extension"
          fedoraProvider.disseminateDatastream(datasetId, streamId)
            .map(addMetadataStreamTo(bag, bagFile))
            .tried.flatten
        }
    }

    def noPayload(selectedForSecondBag: List[FileInfo], selectedForFirstBag: List[FileInfo]): Boolean = {
      if (selectedForFirstBag.size > options.cutoff || selectedForSecondBag.size > options.cutoff) {
        logger.warn(s"too many files ${ selectedForFirstBag.size }, ${ selectedForSecondBag.size }")
        true
      }
      else options.noPayload
    }

    def getInfoFirstBag(allFileInfos: List[FileInfo], emd: Node, hasSecondBag: Boolean): Try[List[FileInfo]] = Try {
      allFileInfos.selectForFirstBag(emd, hasSecondBag, options.europeana, options.noPayload)
    }


    def payloadInEasy(skipPayload: Boolean) = {
      if (skipPayload)
        <dct:description xml:lang="en">{ new PCData("<b>Files not yet migrated to Data Station. For access to these files, please contact DANS at <a href=\"mailto:info@dans.knaw.nl\">info@dans.knaw.nl</a>.</b>") }</dct:description>
      else Text("")
    }
    //        <dct:description xml:lang="en">{ new PCData(s"<b>Files not yet migrated to Data Station. Files for this dataset can be found at ${makelink(datasetId)}.</b>") }</dct:description>

    // <dct:description xml:lang="en">{ new PCData(s"<b>Files not yet migrated to Data Station. For access to these files, please contact DANS at <a href=\"mailto:info@dans.knaw.nl\">info@dans.knaw.nl</a>.</b>") }</dct:description>

    def makelink(datasetId: DatasetId): Node = {
     <a href={ s"https://easy.dans.knaw.nl/ui/datasets/id/$datasetId" }>{ s"https://easy.dans.knaw.nl/ui/datasets/id/$datasetId" }</a>
    }

    for {
      foXml <- fedoraProvider.loadFoXml(datasetId)
      depositor <- getOwner(foXml)
      emdXml <- getEmd(foXml)
      emdString = emdXml.serialize.split("\n").tail.mkString("\n").trim //drop prologue
      emd <- Try(emdUnmarshaller.unmarshal(emdString))
      amd <- getAmd(foXml)
      audiences <- emd.getEmdAudience.getDisciplines.asScala
        .map(id => getAudience(id.getValue)).collectResults
      fedoraFileIDs <- if (options.noPayload) Success(Seq.empty)
                       else fsRdb.getSubordinates(datasetId)
      isOriginalVersioned = options.transformationType == ORIGINAL_VERSIONED
      allFileInfos <- FileInfo(fedoraFileIDs, fedoraProvider, options.withAv).map(_.toList)
      selectedForSecondBag = allFileInfos.selectForSecondBag(isOriginalVersioned, options.noPayload)
      selectedForFirstBag <- getInfoFirstBag(allFileInfos, emdXml, selectedForSecondBag.nonEmpty)
      skipPayload = noPayload(selectedForSecondBag, selectedForFirstBag)
      _ = trace(skipPayload, selectedForFirstBag.size, selectedForSecondBag.size, options.noPayload, options.cutoff)
      (forFirstBag, forSecondBag) <- if (!skipPayload) checkDuplicates(selectedForFirstBag, selectedForSecondBag, isOriginalVersioned)
                                     else Success((Seq.empty, Seq.empty))
      _ = trace("creating DDM from EMD")
      ddm <- DDM(emd, audiences, configuration.abrMapping, payloadInEasy(skipPayload))
      _ = trace("created DDM from EMD")
      maybeFilterViolations <- options.datasetFilter.violations(emd, ddm, amd, selectedForFirstBag, allFileInfos, configuration.exportStates)
      _ = if (options.strict) maybeFilterViolations.foreach(msg => throw InvalidTransformationException(msg))
      _ = (ddm \\ "implemented").filter(_.prefix == "not").foreach(n => throw InvalidTransformationException(n.toString()))
      // so far for collecting data, now we start writing
      _ = logger.info(s"Creating $bagDir from $datasetId with owner $depositor")
      bag <- DansV0Bag.empty(bagDir)
      _ = bag.withEasyUserAccount(depositor).withCreated(DateTime.now())
      _ = maybeFirstBagVersion.map(_.addTo(bag))
      _ <- addXmlMetadataTo(bag, "emd.xml")(emdXml)
      _ <- addXmlMetadataTo(bag, "amd.xml")(amd)
      _ <- getDdm(foXml)
        .map(addXmlMetadataTo(bag, "original/dataset.xml"))
        .getOrElse(Success(()))
      _ <- addXmlMetadataTo(bag, "dataset.xml")(ddm)
      _ <- getMessageFromDepositor(foXml)
        .map(addXmlMetadataTo(bag, "depositor-info/message-from-depositor.txt"))
        .getOrElse(Success(()))
      _ <- getAgreementsXml(foXml)
        .map(addAgreementsTo(bag))
        .getOrElse(AgreementsXml(foXml, ldap)
          .map(addAgreementsTo(bag)))
      _ <- managedMetadataStream(foXml, "ADDITIONAL_LICENSE", bag, "license")
        .getOrElse(Success(()))
      _ <- managedMetadataStream(foXml, "DATASET_LICENSE", bag, "depositor-info/depositor-agreement")
        .getOrElse(Success(()))
      _ <- getFilesXml(foXml)
        .map(addXmlMetadataTo(bag, "original/files.xml"))
        .getOrElse(Success(()))
      fileItemsForFirstBag <- forFirstBag.toList.traverse(addPayloadFileTo(bag, isOriginalVersioned))
      _ <- checkNotImplementedFileMetadata(fileItemsForFirstBag, logger)
      _ <- addXmlMetadataTo(bag, "files.xml")(filesXml(fileItemsForFirstBag))
      _ <- bag.save
      doi = emd.getEmdIdentifier.getDansManagedDoi
      urn = getUrn(datasetId, emd)
    } yield DatasetInfo(maybeFilterViolations, doi, urn, depositor, forSecondBag, !skipPayload)
  }

  private def getUrn(datasetId: DatasetId, emd: EasyMetadataImpl) = {
    emd.getEmdIdentifier.getDcIdentifier.asScala
      .find(_.getScheme == "PID")
      .map(_.getValue)
      .getOrElse(throw InvalidTransformationException(s"no URN in EMD of $datasetId "))
  }

  private def getAudience(id: String) = {
    // TODO cash, or does the fedoraClient that for us?
    fedoraProvider.loadFoXml(id).map(foXml =>
      (foXml \\ "discipline-md" \ "OICode").text
    )
  }

  private def addMetadataStreamTo(bag: DansV0Bag, target: String)(content: InputStream): Try[Any] = {
    bag.addTagFile(content, Paths.get(s"metadata/$target"))
  }

  private def addXmlMetadataTo(bag: DansV0Bag, target: String)(content: Node): Try[Any] = {
    bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/$target"))
  }

  private def addAgreementsTo(bag: DansV0Bag)(content: Node): Try[Any] = {
    bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/depositor-info/agreements.xml"))
  }

  private def addPayloadFileTo(bag: DansV0Bag, isOriginalVersioned: Boolean)(fileInfo: FileInfo): Try[Node] = {
    trace(fileInfo)
    val target = fileInfo.path.bagPath(isOriginalVersioned)

    def saveEmpty = {
      val emptyStream = new ByteArrayInputStream(Array[Byte]())
      bag.addPayloadFile(emptyStream, target)
    }

    def disseminate = {
      val file = bag.baseDir / "data" / target.toString
      for {
        _ <- fedoraProvider
          .disseminateDatastream(fileInfo.fedoraFileId, streamId = "EASY_FILE")
          .map(bag.addPayloadFile(_, target))
          .tried.flatten
        // if fileInfo has a checksum, try to get the same type of checksum from manifest as calculated by addPayload
        maybeBagChecksum = fileInfo.maybeDigestType.flatMap(getChecksum(file, bag))
        _ <- verifyChecksums(file, fileInfo.maybeDigestValue, maybeBagChecksum)
      } yield ()
    }

    for {
      fileItem <- FileItem(fileInfo, isOriginalVersioned)
      _ <- fileInfo.locationUrl.map(_ => Success(saveEmpty)).getOrElse(disseminate)
    } yield fileItem
  }

  private def verifyChecksums(file: File, fedoraValue: Option[String], bagValue: Option[String]) = {
    trace(file, fedoraValue, bagValue)
    (bagValue, fedoraValue) match {
      case (Some(b), Some(f)) if f == b => Success(())
      case (Some(_), Some(_)) => Failure(new Exception(
        s"Different checksums in fedora $fedoraValue and exported bag $bagValue for $file"
      ))
      case (_, None) => logger.warn(s"No checksum in fedora for $file")
        Success(())
      case _ => Failure(new Exception(s"No checksum in bag (or not the same type as in fedora) for $file"))
    }
  }

  private def getChecksum(file: File, bag: DansV0Bag)(digestType: String): Option[String] = {
    val algorithms = Map(
      "SHA-1" -> ChecksumAlgorithm.SHA1,
      "MD-5" -> ChecksumAlgorithm.MD5,
      "SHA-256" -> ChecksumAlgorithm.SHA256,
      "SHA-256" -> ChecksumAlgorithm.SHA512,
    )
    for {
      algorithm <- algorithms.get(digestType)
      manifest <- bag.payloadManifests.get(algorithm)
      checksum <- manifest.get(file)
    } yield checksum
  }
}
