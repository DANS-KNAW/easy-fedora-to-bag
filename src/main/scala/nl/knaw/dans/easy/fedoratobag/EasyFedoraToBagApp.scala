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

  def createExport(input: Iterator[DatasetId], outputDir: File, strict: Boolean, europeana: Boolean, filter: DatasetFilter, outputFormat: OutputFormat)
                  (printer: CSVPrinter): Try[FeedBackMessage] = input.map { datasetId =>
    val bagUuid = UUID.randomUUID.toString
    val sipUuid = UUID.randomUUID.toString
    val sipDir = configuration.stagingDir / sipUuid
    val bagDir = outputFormat match {
      case OutputFormat.AIP => configuration.stagingDir / bagUuid
      case OutputFormat.SIP => sipDir / bagUuid
    }
    val triedCsvRecord = for {
      csvRecord <- createBag(datasetId, bagDir, strict, europeana, filter)
      _ = debug(s"Result from createBag: $csvRecord")
      _ <- Try {
        debug("Moving bag to output dir...")
        outputFormat match {
          case OutputFormat.AIP => bagDir.moveTo(outputDir / bagUuid)(CopyOptions.atomically)
          case OutputFormat.SIP => sipDir.moveTo(outputDir / sipUuid)(CopyOptions.atomically)
        }
      }
    } yield csvRecord
    errorHandling(triedCsvRecord, printer, datasetId, bagDir)
  }.failFastOr(Success("no fedora/IO errors"))

  private def errorHandling(triedCsvRecord: Try[CsvRecord], printer: CSVPrinter, datasetId: DatasetId, ipDir: File) = {
    triedCsvRecord
      .doIfFailure {
        case t: InvalidTransformationException => logger.warn(s"$datasetId -> $ipDir failed: ${ t.getMessage }")
        case t: Throwable => logger.error(s"$datasetId -> $ipDir had a not expected exception: ${ t.getMessage }", t)
      }
      .recoverWith {
        case t: FedoraClientException if t.getStatus != 404 => Failure(t)
        case t: IOException => Failure(t)
        case t => Success(CsvRecord(
          datasetId, UUID.fromString(ipDir.name), doi = "", depositor = "", SIMPLE.toString, s"FAILED: $t"
        ))
      }.doIfSuccess(_.print(printer))
  }

  protected[EasyFedoraToBagApp] def createBag(datasetId: DatasetId, bagDir: File, strict: Boolean, europeana: Boolean, datasetFilter: DatasetFilter): Try[CsvRecord] = {

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
      maybeFilterViolations <- datasetFilter.violations(emd, ddm, amd, fedoraIDs)
      _ = if (strict) maybeFilterViolations.foreach(msg => throw InvalidTransformationException(msg))
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
      fileFilterType = FileFilterType.from(europeana, emdXml)
      fileItems <- addPayloads(bag, fileFilterType, fedoraIDs.filter(_.startsWith("easy-file:")))
      _ <- checkNotImplemented(fileItems, logger)
      _ <- addXmlMetadataTo(bag, "files.xml")(filesXml(fileItems))
      _ <- bag.save()
      doi = emd.getEmdIdentifier.getDansManagedDoi
    } yield CsvRecord(
      datasetId,
      UUID.fromString(bagDir.name),
      doi,
      depositor,
      transformationType = maybeFilterViolations.map(_ => "not strict simple").getOrElse(SIMPLE.toString),
      maybeFilterViolations.getOrElse("OK"),
    )
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

  def addPayloads(bag: DansV0Bag, fileFilterType: FileFilterType, fileIds: Seq[String]): Try[List[Node]] = {
    for {
      allFileInfos <- fileIds.toList.traverse(getFileInfo)
      filteredFileInfos <- selectFileInfos(fileFilterType, allFileInfos)
      fileItems <- filteredFileInfos.traverse(addPayloadFileTo(bag))
    } yield fileItems
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
      if (selected.isEmpty) Failure(NoPayloadFilesException())
      else Success(List(selected.maxBy(_.size)))
    }

    fileFilterType match {
      case LARGEST_PDF => largest(LARGEST_PDF, LARGEST_IMAGE)
      case LARGEST_IMAGE => largest(LARGEST_IMAGE, LARGEST_PDF)
      case ALL_FILES => if (fileInfos.isEmpty) Failure(NoPayloadFilesException())
                        else Success(fileInfos)
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
