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

import java.io.{ IOException, InputStream }
import java.nio.file.Paths
import java.util.UUID

import better.files.File.CopyOptions
import better.files.{ File, StringExtensions }
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import com.yourmediashelf.fedora.client.{ FedoraClient, FedoraClientException }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.bag.ChecksumAlgorithm
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedoratobag.Command.FeedBackMessage
import nl.knaw.dans.easy.fedoratobag.FileFilterType.{ LARGEST_IMAGE, _ }
import nl.knaw.dans.easy.fedoratobag.FileItem.{ checkNotImplemented, filesXml }
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

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node }

class EasyFedoraToBagApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  lazy val bagIndex: BagIndex = filter.BagIndex(configuration.bagIndexUrl)
  private lazy val ldap = new Ldap(ldapContext)
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def createExport(input: Iterator[DatasetId], outputDir: File, options: Options, outputFormat: OutputFormat)
                  (printer: CSVPrinter): Try[FeedBackMessage] = input.map { datasetId =>

    def movePackageAtomically(packageDir: File) = {
      val target = outputDir / packageDir.name
      debug(s"Moving $outputFormat to output dir: $target")
      Try(packageDir.moveTo(target)(CopyOptions.atomically))
    }

    def bagDir(packageDir: File) = outputFormat match {
      case OutputFormat.AIP => packageDir
      case OutputFormat.SIP => packageDir / UUID.randomUUID.toString
    }

    val packageUuid1 = UUID.randomUUID
    val packageDir1 = configuration.stagingDir / packageUuid1.toString
    val bagDir1 = bagDir(packageDir1)
    val packageUuid2 = UUID.randomUUID
    val packageDir2 = configuration.stagingDir / packageUuid2.toString
    val triedCsvRecord = for {
      datasetInfo <- createFirstBag(datasetId, bagDir1, options)
      bagDir2 = if (datasetInfo.nextFileInfos.isEmpty) None
                else  Some(bagDir(packageDir2))
      _ <- bagDir2.map(createSecondBag(datasetInfo.nextFileInfos, bagDir1)).getOrElse(Success(()))
      // the 2nd bag is moved first, thus a next process can stumble over a missing first bag in case of interrupts
      _ <- bagDir2.map(_ => movePackageAtomically(packageDir2)).getOrElse(Success(()))
      _ <- movePackageAtomically(packageDir1)
    } yield CsvRecord(
      datasetId,
      packageUuid1,
      bagDir2.map(_ => packageUuid2),
      datasetInfo.doi,
      datasetInfo.depositor,
      transformationType = datasetInfo.maybeFilterViolations.map(_ => "not strict simple").getOrElse(SIMPLE.toString),
      datasetInfo.maybeFilterViolations.getOrElse("OK"),
    )
    errorHandling(triedCsvRecord, printer, datasetId, packageDir1)
  }.failFastOr(Success("no fedora/IO errors"))

  private def errorHandling(triedCsvRecord: Try[CsvRecord], printer: CSVPrinter, datasetId: DatasetId, packageDir: File) = {
    triedCsvRecord
      .doIfFailure {
        case t: InvalidTransformationException => logger.warn(s"$datasetId -> $packageDir failed: ${ t.getMessage }")
        case t: Throwable => logger.error(s"$datasetId -> $packageDir had a not expected exception: ${ t.getMessage }", t)
      }
      .recoverWith {
        case t: FedoraClientException if t.getStatus != 404 => Failure(t)
        case t: IOException => Failure(t)
        case t => Success(CsvRecord(
          datasetId, UUID.fromString(packageDir.name), None, doi = "", depositor = "", SIMPLE.toString, s"FAILED: $t"
        ))
      }.doIfSuccess(_.print(printer))
  }

  protected[EasyFedoraToBagApp] def createSecondBag(fileInfos: Seq[FileInfo], bagDir1: File)(bagDir2: File): Try[Unit] = {
    throw new Exception("2nd dataset not yet implemented")
  }

  protected[EasyFedoraToBagApp] def createFirstBag(datasetId: DatasetId, bagDir: File, options: Options): Try[DatasetInfo] = {

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
        .map(_.withEasyUserAccount(depositor).withCreated(DateTime.now()))
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
      allFileInfos <- fedoraIDs.filter(_.startsWith("easy-file:")).toList.traverse(getFileInfo)
      firstFileInfos <- selectFileInfos(options.firstFileFilter(emdXml), allFileInfos)
      firstBagFileItems <- firstFileInfos.traverse(addPayloadFileTo(bag))
      _ <- checkNotImplemented(firstBagFileItems, logger)
      _ <- addXmlMetadataTo(bag, "files.xml")(filesXml(firstBagFileItems))
      _ <- bag.save()
      doi = emd.getEmdIdentifier.getDansManagedDoi
      nextFileInfos = if (maybeFilterViolations.nonEmpty && options.strict) Seq.empty
                      else getNextFileInfos(allFileInfos, firstFileInfos, options.originalVersioning)
    } yield DatasetInfo(maybeFilterViolations, doi, depositor, nextFileInfos)
  }

  private def getNextFileInfos(allFileInfos: List[FileInfo], firstFileInfos: List[FileInfo], originalVersioning: Boolean): Seq[FileInfo] = {
    if (!originalVersioning || allFileInfos.size == firstFileInfos.size)
      Seq[FileInfo]()
    else {
      val notAccessibleOriginals = selectFileInfos(NOT_ACCESSIBLE, firstFileInfos).getOrElse(Seq.empty)
      val nextFileInfos = allFileInfos.toSet &~ notAccessibleOriginals.toSet
      logger.debug(s"nextFileInfos = ${ nextFileInfos.map(_.path) }")
      nextFileInfos.toSeq
    }
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
      case ORIGINAL_FILES => successUnlessEmpty(fileInfos.filter(_.path.startsWith("original/")))
      case NOT_ACCESSIBLE => successUnlessEmpty(fileInfos.filter(_.accessibleTo == "NONE"))
      case ALL_FILES => successUnlessEmpty(fileInfos)
    }
  }

  private def getFileInfo(fedoraFileId: String): Try[FileInfo] = {
    fedoraProvider
      .loadFoXml(fedoraFileId)
      .flatMap(FileInfo(_))
      .recoverWith {
        case t: Throwable => Failure(new Exception(s"$fedoraFileId ${ t.getMessage }"))
      }
  }

  private def addPayloadFileTo(bag: DansV0Bag)(fileInfo: FileInfo): Try[Node] = {
    val streamId = "EASY_FILE"
    for {
      fileItem <- FileItem(fileInfo)
      _ <- fedoraProvider
        .disseminateDatastream(fileInfo.fedoraFileId, streamId)
        .map(bag.addPayloadFile(_, fileInfo.path))
        .tried.flatten
      _ <- bag.save()
      _ = fileInfo.contentDigest.map(validateChecksum(bag.baseDir / s"data/${ fileInfo.path }", bag, fileInfo.fedoraFileId))
        .getOrElse(Success(logger.warn(s"No digest found for ${ fileInfo.fedoraFileId } path = ${ fileInfo.path }")))
    } yield fileItem
  }.recoverWith { case e => Failure(new Exception(s"${ fileInfo.fedoraFileId } ${ e.getMessage }", e)) }

  private def validateChecksum(file: File, bag: DansV0Bag, fedoraFileId: String)(maybeDigest: Node) = Try {
    val algorithms = Map(
      "SHA-1" -> ChecksumAlgorithm.SHA1,
      "MD-5" -> ChecksumAlgorithm.MD5,
      "SHA-256" -> ChecksumAlgorithm.SHA256,
      "SHA-256" -> ChecksumAlgorithm.SHA512,
    )
    val digestType = (maybeDigest \\ "@TYPE").text
    val digestValue = (maybeDigest \\ "@DIGEST").text
    val checksum = (for {
      algorithm <- algorithms.get(digestType)
      manifest <- bag.payloadManifests.get(algorithm)
      checksum <- manifest.get(file)
    } yield checksum)
      .getOrElse(throw new Exception(s"Could not find digest [$digestType/$digestValue] for $fedoraFileId $file in manifest"))
    if (checksum != digestValue)
      throw new Exception(s"checksum error fedora[$digestValue] bag[$checksum] $fedoraFileId $file")
  }
}
