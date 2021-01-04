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
import nl.knaw.dans.easy.fedoratobag.FileItem.{ checkNotImplementedFileMetadata, filesXml }
import nl.knaw.dans.easy.fedoratobag.FoXml.{ getEmd, _ }
import nl.knaw.dans.easy.fedoratobag.OutputFormat.OutputFormat
import nl.knaw.dans.easy.fedoratobag.TransformationType._
import nl.knaw.dans.easy.fedoratobag.filter.FileFilterType.{ LARGEST_IMAGE, _ }
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
import scala.xml.{ Comment, Elem, Node }

class EasyFedoraToBagApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  lazy val bagIndex: BagIndex = filter.BagIndex(configuration.bagIndexUrl)
  private lazy val ldap = new Ldap(ldapContext)
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def createSequences(lines: Iterator[String], outputDir: File, options: Options)(printer: CSVPrinter): Try[FeedBackMessage] = {
    logger.info(options.toString)

    def exportBag(firstVersion: Option[VersionInfo], datasetId: DatasetId): Try[VersionInfo] = {
      val packageUUID = UUID.randomUUID
      val packageDir = configuration.stagingDir / packageUUID.toString
      val csvUuid1 = firstVersion.map(_.packageId).getOrElse(packageUUID)
      val csvUuid2 = firstVersion.map(_ => packageUUID)
      for {
        datasetInfo <- createBag(datasetId, packageDir / UUID.randomUUID.toString, options, firstVersion)
        _ <- movePackageAtomically(packageDir, outputDir)
        thisVersionInfo = VersionInfo(datasetInfo, packageUUID)
        _ <- CsvRecord(datasetId, datasetInfo, csvUuid1, csvUuid2, options).print(printer)
      } yield thisVersionInfo
    }

    def exportWithRecover(firstVersion: VersionInfo)(datasetId: DatasetId): Try[Any] = {
      val tried = exportBag(Some(firstVersion), datasetId)
      errorHandling(tried, printer, datasetId, firstVersion.packageId)
    }

    def exportSequence(datasetIds: Array[Depositor])(firstDatasetId: DatasetId) = for {
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

  def createExport(input: Iterator[DatasetId], outputDir: File, options: Options, outputFormat: OutputFormat)
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
    val triedCsvRecord = for {
      datasetInfo <- createBag(datasetId, bagDir1, options)
      maybeBag2 <- if (datasetInfo.nextFileInfos.isEmpty) Success(None)
                   else DansV0Bag.empty(bagDir2).map(Some(_))
      _ <- maybeBag2.map(fillSecondBag(datasetInfo, bagDir1, packageUuid1)).getOrElse(Success(()))
      // the 2nd bag is moved first, thus a next process has a chance to stumble over a missing first bag in case of interrupts
      _ <- maybeBag2.map(_ => movePackageAtomically(packageDir2, outputDir)).getOrElse(Success(()))
      _ <- movePackageAtomically(packageDir1, outputDir)
      maybeUuid2 = maybeBag2.map(_ => packageUuid2)
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

  private def fillSecondBag(datasetInfo: DatasetInfo, bagDir1: File, isVersionOf: UUID)(bag2: DansV0Bag): Try[Unit] = {

    def copy(fileName: String) = {
      (bagDir1 / "metadata" / fileName)
        .inputStream
        .map(addMetadataStreamTo(bag2, fileName))
        .get
    }

    bag2
      .withEasyUserAccount(datasetInfo.depositor)
      .withCreated(DateTime.now())
      .withIsVersionOf(isVersionOf)
      // the following keys should match easy-fedora-to-bag
      .addBagInfo("Base-DOI", datasetInfo.doi)
      .addBagInfo("Base-URN", datasetInfo.urn)
    for {
      _ <- copy("emd.xml")
      _ <- copy("amd.xml")
      _ <- copy("dataset.xml")
      _ <- (bagDir1 / "metadata").list.toList
        .filter(_.name.toLowerCase.contains("license"))
        .traverse(file => copy(file.name))
      fileItems <- datasetInfo.nextFileInfos.toList.traverse(addPayloadFileTo(bag2, isOriginalVersioned = true))
      _ <- checkNotImplementedFileMetadata(fileItems, logger)
      _ <- createFilesXml(datasetInfo.nextFileInfos, fileItems, bag2)
      _ <- bag2.save
    } yield ()
  }

  def createBag(datasetId: DatasetId, bagDir: File, options: Options, firstVersionInfo: Option[VersionInfo] = None): Try[DatasetInfo] = {

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

    for {
      foXml <- fedoraProvider.loadFoXml(datasetId)
      depositor <- getOwner(foXml)
      emdXml <- getEmd(foXml)
      emd <- Try(emdUnmarshaller.unmarshal(emdXml.serialize))
      amd <- getAmd(foXml)
      audiences <- emd.getEmdAudience.getDisciplines.asScala
        .map(id => getAudience(id.getValue)).collectResults
      ddm <- DDM(emd, audiences, configuration.abrMapping)
      fedoraIDs <- fedoraProvider.getSubordinates(datasetId)
      maybeFilterViolations <- options.datasetFilter.violations(emd, ddm, amd, fedoraIDs)
      _ = if (options.strict) maybeFilterViolations.foreach(msg => throw InvalidTransformationException(msg))
      _ = logger.info(s"Creating $bagDir from $datasetId with owner $depositor")
      bag <- DansV0Bag.empty(bagDir)
      _ = bag.withEasyUserAccount(depositor).withCreated(DateTime.now())
      _ = firstVersionInfo.map(_.addVersionOf(bag))
      _ <- addXmlMetadataTo(bag, "emd.xml")(emdXml)
      _ <- addXmlMetadataTo(bag, "amd.xml")(amd)
      _ <- getDdm(foXml)
        .map(addXmlMetadataTo(bag, "original/dataset.xml"))
        .getOrElse(Success(()))
      _ <- addXmlMetadataTo(bag, "dataset.xml")(ddm)
      _ <- getMessageFromDepositor(foXml)
        .map(addXmlMetadataTo(bag, "depositor-info/message-from-depositor.txt"))
        .getOrElse(Success(()))
      _ <- getFilesXml(foXml)
        .map(addXmlMetadataTo(bag, "original/files.xml"))
        .getOrElse(Success(()))
      _ <- getAgreementsXml(foXml)
        .map(addAgreementsTo(bag))
        .getOrElse(AgreementsXml(foXml, ldap)
          .map(addAgreementsTo(bag)))
      _ <- managedMetadataStream(foXml, "ADDITIONAL_LICENSE", bag, "license")
        .getOrElse(Success(()))
      _ <- managedMetadataStream(foXml, "DATASET_LICENSE", bag, "depositor-info/depositor-agreement")
        .getOrElse(Success(()))
      isOriginalVersioned = options.transformationType == ORIGINAL_VERSIONED
      allFileInfos <- fedoraIDs.filter(_.startsWith("easy-file:")).toList.traverse(getFileInfo)
      nextFileInfos = forSecondBag(allFileInfos, isOriginalVersioned)
      filterType = firstFileFilter(emdXml, nextFileInfos.nonEmpty, options.europeana)
      firstFileInfos <- selectFileInfos(filterType, allFileInfos)
      _ = logger.debug(s"nextFileInfos = ${ nextFileInfos.map(_.path) }")
      firstBagFileItems <- firstFileInfos.traverse(addPayloadFileTo(bag, isOriginalVersioned))
      _ <- checkNotImplementedFileMetadata(firstBagFileItems, logger)
      _ <- createFilesXml(firstFileInfos, firstBagFileItems, bag)
      _ <- bag.save
      doi = emd.getEmdIdentifier.getDansManagedDoi
      urn = getUrn(datasetId, emd)
    } yield DatasetInfo(maybeFilterViolations, doi, urn, depositor, nextFileInfos)
  }

  private def forSecondBag(all: Seq[FileInfo], isOriginalVersioned: Boolean): Seq[FileInfo] = {
    if (!isOriginalVersioned) Seq.empty
    else {
      val accessibleOriginals = all.filter(_.isAccessibleOriginal)
      if (all.size == accessibleOriginals.size) Seq.empty
      else accessibleOriginals ++ all.filterNot(_.isOriginal)
    }
  }

  private def dcmiType(emd: Node): String = {
    def hasDcmiScheme(node: Node) = node
      .attribute("http://easy.dans.knaw.nl/easy/easymetadata/eas/", "scheme")
      .exists(_.text == "DCMI")

    (emd \ "type" \ "type")
      .filter(hasDcmiScheme)
      .text.toLowerCase.trim
  }

  private def firstFileFilter(emd: Node, hasSecondBag: Boolean, europeana: Boolean): FileFilterType = {
    if (hasSecondBag) ORIGINAL_FILES
    else if (!europeana) ALL_FILES
         else if (dcmiType(emd) == "text") LARGEST_PDF
              else LARGEST_IMAGE
  }

  private def selectFileInfos(fileFilterType: FileFilterType, fileInfos: List[FileInfo]): Try[List[FileInfo]] = {

    def largest(by: FileFilterType, orElseBy: FileFilterType): Try[List[FileInfo]] = {
      val infosByType = fileInfos
        .filter(_.accessibleTo == "ANONYMOUS")
        .groupBy(fi => if (fi.mimeType.startsWith("image/")) LARGEST_IMAGE
                       else if (fi.mimeType.startsWith("application/pdf")) LARGEST_PDF
                            else ALL_FILES
        )
      val selected = infosByType.getOrElse(by, infosByType.getOrElse(orElseBy, List.empty))
      maxSizeUnlessEmpty(selected)
    }

    def maxSizeUnlessEmpty(selected: List[FileInfo]) = {
      if (selected.isEmpty) Failure(NoPayloadFilesException())
      else Success(List(selected.maxBy(_.size)))
    }

    def successUnlessEmpty(fileInfos: List[FileInfo]) = {
      if (fileInfos.isEmpty) Failure(NoPayloadFilesException())
      else Success(fileInfos)
    }

    fileFilterType match {
      case LARGEST_PDF => largest(LARGEST_PDF, LARGEST_IMAGE)
      case LARGEST_IMAGE => largest(LARGEST_IMAGE, LARGEST_PDF)
      case ORIGINAL_FILES => successUnlessEmpty(fileInfos.filter(_.isOriginal)) // TODO is ALL_FILES if no second bag
      case ALL_FILES => successUnlessEmpty(fileInfos)
    }
  }

  private def getUrn(datasetId: DatasetId, emd: EasyMetadataImpl) = {
    emd.getEmdIdentifier.getDcIdentifier.asScala
      .find(_.getScheme == "PID")
      .map(_.getValue)
      .getOrElse(throw new Exception(s"no URN in EMD of $datasetId "))
  }

  private def getAudience(id: String) = {
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

  private def getFileInfo(fedoraFileId: String): Try[FileInfo] = {
    fedoraProvider
      .loadFoXml(fedoraFileId)
      .flatMap(FileInfo(_))
      .recoverWith {
        case t: Throwable => Failure(new Exception(s"$fedoraFileId ${ t.getMessage }"))
      }
  }

  private def addPayloadFileTo(bag: DansV0Bag, isOriginalVersioned: Boolean)(fileInfo: FileInfo): Try[Node] = {
    val target = fileInfo.bagPath(isOriginalVersioned)
    val file = bag.baseDir / "data" / target.toString
    val streamId = "EASY_FILE"

    // lazy so the else branch can add the file to the bag first
    lazy val maybeBagChecksum = fileInfo.maybeDigestType.flatMap(getChecksum(file, bag))
    val maybeFedoraChecksum = fileInfo.maybeDigestValue

    if (file.exists) verifyNameClash(file, maybeFedoraChecksum, maybeBagChecksum).flatMap { _ =>
      FileItem(fileInfo, isOriginalVersioned).map(n => Comment(n.toOneLiner))
    }
    else for {
      fileItem <- FileItem(fileInfo, isOriginalVersioned)
      _ <- fedoraProvider
        .disseminateDatastream(fileInfo.fedoraFileId, streamId)
        .map(bag.addPayloadFile(_, target))
        .tried.flatten
      _ <- verifyChecksums(file, maybeFedoraChecksum, maybeBagChecksum)
    } yield fileItem
  }

  private def createFilesXml(fileInfos: Seq[FileInfo], fileItems: Seq[Node], bag: DansV0Bag) = {
    val actualItems = fileItems.filterNot(_.isInstanceOf[Comment])
    val nrOfDuplicates = fileInfos.size - actualItems.size
    if (nrOfDuplicates != 0)
      logger.warn(s"$nrOfDuplicates duplicate file(s), see comment(s) in files.xml of ${ bag.name }")
    // TODO merge skipped fileInfos into fileItems?
    //  See https://drivenbydata.atlassian.net/browse/EASY-2808
    addXmlMetadataTo(bag, "files.xml")(filesXml(fileItems))
  }

  private def verifyNameClash(file: File, fedoraValue: Option[String], bagValue: Option[String]) = {
    (bagValue, fedoraValue) match {
      case (Some(b), Some(f)) if f == b =>
        Success(())
      case (Some(_), Some(_)) => Failure(new Exception(
        s"Duplicate file paths with different checksums. Current $fedoraValue previous $bagValue, $file"
      ))
      case _ => Failure(new Exception(
        s"Duplicate file paths, no checksum in fedora to verify for $file"
      ))
    }
  }

  private def verifyChecksums(file: File, fedoraValue: Option[String], bagValue: Option[String]) = {
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
