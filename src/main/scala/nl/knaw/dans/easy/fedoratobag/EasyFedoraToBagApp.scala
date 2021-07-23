/**
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
import nl.knaw.dans.easy.fedoratobag.FoXml.{ getEmd, _ }
import nl.knaw.dans.easy.fedoratobag.OutputFormat.OutputFormat
import nl.knaw.dans.easy.fedoratobag.TransformationType._
import nl.knaw.dans.easy.fedoratobag.filter._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.apache.commons.csv.CSVPrinter
import org.joda.time.DateTime

import java.io.{ IOException, InputStream }
import java.nio.file.Paths
import java.util.UUID
import javax.naming.ldap.InitialLdapContext
import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node, Text }

class EasyFedoraToBagApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val fsRdb = new FsRdb(configuration.databaseConnection)
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  lazy val bagIndex: BagIndex = filter.BagIndex(configuration.bagIndexUrl)
  private lazy val ldap = new Ldap(ldapContext)
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def createSequences(lines: Iterator[String], outputDir: File, options: Options)(printer: CSVPrinter): Try[FeedBackMessage] = {
    logger.info(options.toString)

    def exportBag(firstVersion: Option[BagVersion], datasetId: DatasetId): Try[BagVersion] = {
      val packageUUID = UUID.randomUUID
      val packageDir = configuration.stagingDir / packageUUID.toString
      val csvUuid1 = firstVersion.map(_.packageId).getOrElse(packageUUID)
      val csvUuid2 = firstVersion.map(_ => packageUUID)
      for {
        datasetInfo <- createBag(datasetId, packageDir / UUID.randomUUID.toString, options, firstVersion)
        _ <- movePackageAtomically(packageDir, outputDir)
        thisVersionInfo = BagVersion(datasetInfo, packageUUID)
        _ <- CsvRecord(datasetId, datasetInfo, csvUuid1, csvUuid2, options).print(printer)
      } yield thisVersionInfo
    }

    def exportWithRecover(firstVersion: BagVersion)(datasetId: DatasetId): Try[Any] = {
      val tried = exportBag(Some(firstVersion), datasetId)
      errorHandling(tried, printer, datasetId, firstVersion.packageId)
    }

    def exportSequence(datasetIds: Array[DatasetId])(firstDatasetId: DatasetId) = for {
      versionInfo <- exportBag(None, firstDatasetId)
      _ <- datasetIds
        .map(exportWithRecover(versionInfo))
        .toSeq.failFastOr(Success(()))
    } yield ()

    lines.map { line =>
      val datasetIds = line.split(",")
      val triedUnit = datasetIds
        .headOption
        .map(exportSequence(datasetIds.drop(1)))
        .getOrElse(Success(()))
      errorHandling(triedUnit, printer, datasetIds.head, null)
    }.failFastOr(Success("no fedora/IO errors"))
  }

  def createOriginalVersionedExport(input: Iterator[DatasetId], outputDir: File, options: Options, outputFormat: OutputFormat)
                                   (printer: CSVPrinter): Try[FeedBackMessage] = input.map { datasetId =>
    logger.info(options.toString)

    def bagDir(packageDir: File) = outputFormat match {
      case OutputFormat.AIP => packageDir
      case OutputFormat.SIP => packageDir / UUID.randomUUID.toString
    }

    val packageUuid1 = UUID.randomUUID
    val packageUuid2 = UUID.randomUUID
    val packageDir1 = configuration.stagingDir / packageUuid1.toString
    val packageDir2 = configuration.stagingDir / packageUuid2.toString
    val bagDir1 = bagDir(packageDir1)
    val bagDir2 = bagDir(packageDir2)

    def createSecondBag(datasetInfo: DatasetInfo) = {
      if (datasetInfo.nextBagFileInfos.isEmpty) Success(None)
      else for {
        bag2 <- DansV0Bag.empty(bagDir2)
        _ = bag2.withEasyUserAccount(datasetInfo.depositor).withCreated(DateTime.now())
        _ = BagVersion(datasetInfo.doi, datasetInfo.urn, packageUuid1)
          .addTo(bag2)
        _ <- fillSecondBag(bag2, bagDir1 / "metadata", datasetInfo.nextBagFileInfos.toList)
        _ <- movePackageAtomically(packageDir2, outputDir)
      } yield Some(packageUuid2)
    }

    val triedCsvRecord = for {
      datasetInfo <- createBag(datasetId, bagDir1, options)
      maybeUuid2 <- createSecondBag(datasetInfo)
      // first bag moved after second, thus a next process can stumble over a missing first bag in case of interrupts
      _ <- movePackageAtomically(packageDir1, outputDir)
      _ <- CsvRecord(datasetId, datasetInfo, packageUuid1, maybeUuid2, options).print(printer)
    } yield ()
    errorHandling(triedCsvRecord, printer, datasetId, packageUuid1)
  }.failFastOr(Success("no fedora/IO errors"))

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
      case t => CsvRecord(datasetId, packageUUID, None, doi = "", depositor = "", "-", s"FAILED: $t")
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

    def hasTooManyFiles(selectedForSecondBag: List[FileInfo], selectedForFirstBag: List[FileInfo]) = {
      if (!(selectedForFirstBag.size > options.cutoff) && !(selectedForSecondBag.size > options.cutoff))
        !options.noPayload
      else {
        logger.warn(s"too many files ${selectedForFirstBag.size}, ${selectedForSecondBag.size}")
        false
      }
    }

    def payloadInEasy(tooManyFiles: Boolean) = {
      if (tooManyFiles)
        <ddm:description xml:lang="en">{ s"Files for this dataset can be found at https://easy.dans.knaw.nl/ui/datasets/id/$datasetId/tab/2" }</ddm:description>
      else Text("")
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
      fedoraIDs <- if (options.noPayload) Success(List.empty)
                   else fsRdb.getSubordinates(datasetId)
      allFileInfos <- FileInfo(fedoraIDs.filter(_.startsWith("easy-file:")).toList, fedoraProvider).map(_.toList)
      isOriginalVersioned = options.transformationType == ORIGINAL_VERSIONED
      selectedForSecondBag = allFileInfos.selectForSecondBag(isOriginalVersioned, options.noPayload)
      selectedForFirstBag <- allFileInfos.selectForFirstBag(emdXml, selectedForSecondBag.nonEmpty, options.europeana, options.noPayload)
      tooManyFiles = !hasTooManyFiles(selectedForSecondBag, selectedForFirstBag)
      _ = trace(tooManyFiles, selectedForFirstBag.size, selectedForSecondBag.size, options.noPayload, options.cutoff)
      _ = trace("creating DDM from EMD")
      ddm <- DDM(emd, audiences, configuration.abrMapping, payloadInEasy(tooManyFiles))
      _ = trace("created DDM from EMD")
      maybeFilterViolations <- options.datasetFilter.violations(emd, ddm, amd, fedoraIDs, allFileInfos)
      _ = if (options.strict) maybeFilterViolations.foreach(msg => throw InvalidTransformationException(msg))
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
      // TODO one more action to move up before creating the bag. It would break a test and we can live with it for now.
      (forFirstBag, forSecondBag) <- if (!tooManyFiles) checkDuplicates(selectedForFirstBag, selectedForSecondBag, isOriginalVersioned)
                                     else Success((Seq.empty, Seq.empty))
      fileItemsForFirstBag <- forFirstBag.toList.traverse(addPayloadFileTo(bag, isOriginalVersioned))
      _ <- checkNotImplementedFileMetadata(fileItemsForFirstBag, logger)
      _ <- addXmlMetadataTo(bag, "files.xml")(filesXml(fileItemsForFirstBag))
      _ <- bag.save
      doi = emd.getEmdIdentifier.getDansManagedDoi
      urn = getUrn(datasetId, emd)
    } yield DatasetInfo(maybeFilterViolations, doi, urn, depositor, forSecondBag, !tooManyFiles)
  }

  private def getUrn(datasetId: DatasetId, emd: EasyMetadataImpl) = {
    emd.getEmdIdentifier.getDcIdentifier.asScala
      .find(_.getScheme == "PID")
      .map(_.getValue)
      .getOrElse(throw new Exception(s"no URN in EMD of $datasetId "))
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
    val target = fileInfo.bagPath(isOriginalVersioned)
    val file = bag.baseDir / "data" / target.toString
    for {
      fileItem <- FileItem(fileInfo, isOriginalVersioned)
      _ <- fedoraProvider
        .disseminateDatastream(fileInfo.fedoraFileId, streamId = "EASY_FILE")
        .map(bag.addPayloadFile(_, target))
        .tried.flatten
      maybeBagChecksum = fileInfo.maybeDigestType.flatMap(getChecksum(file, bag))
      _ <- verifyChecksums(file, fileInfo.maybeDigestValue, maybeBagChecksum)
    } yield fileItem
  }

  private def verifyChecksums(file: File, fedoraValue: Option[String], bagValue: Option[String]) = {
    trace(file, fedoraValue, bagValue)
    (bagValue, fedoraValue) match {
      case (Some(b), Some(f)) if f == b => Success(())
      case (Some(_), Some(_)) => Failure(new Exception(
        s"Different checksums in fedora $fedoraValue and exported bag $bagValue for $file"
      ))
      case _ => logger.warn(s"No checksum in fedora for $file")
        Success(())
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
