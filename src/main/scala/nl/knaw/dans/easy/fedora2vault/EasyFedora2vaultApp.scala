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
package nl.knaw.dans.easy.fedora2vault

import java.io.{ IOException, InputStream, Writer }
import java.nio.file.Paths
import java.util.UUID

import better.files.{ Dispose, File, StringExtensions }
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import com.yourmediashelf.fedora.client.{ FedoraClient, FedoraClientException }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.bag.ChecksumAlgorithm
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedora2vault.Command.FeedBackMessage
import nl.knaw.dans.easy.fedora2vault.FileItem.filesXml
import nl.knaw.dans.easy.fedora2vault.FoXml.{ getEmd, _ }
import nl.knaw.dans.easy.fedora2vault.TransformationType.SIMPLE
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.apache.commons.csv.CSVPrinter
import org.joda.time.DateTime

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node, PrettyPrinter }

class EasyFedora2vaultApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  private lazy val ldap = new Ldap(ldapContext)
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def simpleTransForms(datasetIds: Iterator[DatasetId], outputDir: File, writer: Writer): Try[FeedBackMessage] = {
    new Dispose(CsvRecord.csvFormat.print(writer))
      .apply(simpleTransForms(datasetIds, outputDir))
  }

  private def simpleTransForms(input: Iterator[DatasetId], outputDir: File)
                              (printer: CSVPrinter): Try[FeedBackMessage] = input
    .map(simpleTransform(_, outputDir / UUID.randomUUID.toString, printer))
    .failFastOr(Success("OK"))

  private def simpleTransform(datasetId: DatasetId, bagDir: File, printer: CSVPrinter): Try[Any] = {
    simpleTransform(datasetId, bagDir)
      .doIfFailure { case t => logger.error(s"$datasetId -> $datasetId failed: $t", t) }
      .recoverWith {
        case t: FedoraClientException if t.getStatus != 404 => Failure(t)
        case t: Exception if t.isInstanceOf[IOException] => Failure(t)
        case t => Success(CsvRecord(
          datasetId, doi = "", depositor = "", SIMPLE, UUID.fromString(bagDir.name), s"FAILED: $t"
        ))
      }
      .doIfSuccess(_.print(printer))
  }

  def simpleTransform(datasetId: DatasetId, bagDir: File): Try[CsvRecord] = {

    def managedMetadataStream(foXml: Elem, streamId: String, bag: DansV0Bag, metadataFile: String) = {
      managedStreamLabel(foXml, streamId)
        .map { label =>
          val extension = label.split("[.]").last
          val bagFile = s"$metadataFile.$extension"
          fedoraProvider.disseminateDatastream(datasetId, streamId)
            .map(addMetadataStream(bag, bagFile))
            .tried.flatten
        }
    }

    for {
      foXml <- fedoraProvider.loadFoXml(datasetId)
      depositor <- getOwner(foXml)
      msg = s"$bagDir from $datasetId with owner $depositor"
      _ = logger.info("Created " + msg)
      bag <- DansV0Bag.empty(bagDir)
        .map(_.withEasyUserAccount(depositor).withCreated(DateTime.now()))
      emdXml <- getEmd(foXml)
      _ <- addXmlMetadata(bag, "emd.xml")(emdXml)
      amd <- getAmd(foXml)
      _ <- addXmlMetadata(bag, "amd.xml")(amd)
      _ <- getDdm(foXml)
        .map(addXmlPayload(bag, "original-ddm.xml"))
        .getOrElse(Success(()))
      emd <- Try(emdUnmarshaller.unmarshal(emdXml.serialize))
      audiences <- emd.getEmdAudience.getDisciplines.asScala
        .map(id => getAudience(id.getValue)).collectResults
      ddm <- DDM(emd, audiences)
      _ <- addXmlMetadata(bag, "dataset.xml")(ddm)
      _ <- getMessageFromDepositor(foXml)
        .map(addXmlMetadata(bag, "depositor-info/message-from-depositor.txt"))
        .getOrElse(Success(())) // TODO EASY-2697: EMD/other/remark
      _ <- getFilesXml(foXml)
        .map(addXmlPayload(bag, "original-files.xml"))
        .getOrElse(Success(()))
      _ <- getAgreementsXml(foXml)
        .map(addAgreements(bag))
        .getOrElse(AgreementsXml(foXml, ldap)
          .map(addAgreements(bag)))
      _ <- managedMetadataStream(foXml, "ADDITIONAL_LICENSE", bag, "license") // TODO EASY-2696 where to store?
        .getOrElse(Success(()))
      _ <- managedMetadataStream(foXml, "DATASET_LICENSE", bag, "depositor-info/depositor-agreement") // TODO EASY-2697: older versions
        .getOrElse(Success(()))
      fedoraIDs <- fedoraProvider.getSubordinates(datasetId)
      fileItems <- fedoraIDs.filter(_.startsWith("easy-file:"))
        .toList.traverse(addPayloadFileTo(bag))
      _ <- addXmlMetadata(bag, "files.xml")(filesXml(fileItems))
      _ <- bag.save()
      doi = emd.getEmdIdentifier.getDansManagedDoi
    } yield CsvRecord(datasetId, doi, depositor, SIMPLE, UUID.fromString(bagDir.name), "OK")
  }

  private def getAudience(id: String) = {
    fedoraProvider.loadFoXml(id).map(foXml =>
      (foXml \\ "discipline-md" \ "OICode").text
    )
  }

  private def addMetadataStream(bag: DansV0Bag, target: String)(content: InputStream): Try[Any] = {
    bag.addTagFile(content, Paths.get(s"metadata/$target"))
  }

  private def addXmlMetadata(bag: DansV0Bag, target: String)(content: Node): Try[Any] = {
    bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/$target"))
  }

  private def addAgreements(bag: DansV0Bag)(content: Node): Try[Any] = {
    bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/depositor-info/agreements.xml"))
  }

  private def addXmlPayload(bag: DansV0Bag, path: String)(content: Node): Try[Any] = {
    bag.addPayloadFile(content.serialize.inputStream, Paths.get(path))
  }

  // prints a one liner unlike the default printer
  private val logPrinter = new PrettyPrinter(-1, 0)

  private def addPayloadFileTo(bag: DansV0Bag)(fedoraFileId: String): Try[FileItem] = {
    val streamId = "EASY_FILE"
    for {
      foXml <- fedoraProvider.loadFoXml(fedoraFileId)
      path = Paths.get((foXml \\ "file-item-md" \\ "path").text)
      _ = logger.info(s"Adding $fedoraFileId to $path")
      _ <- fedoraProvider
        .disseminateDatastream(fedoraFileId, streamId)
        .map(bag.addPayloadFile(_, path))
        .tried.flatten
      _ <- bag.save()
      fileStream = getStreamRoot(streamId, foXml)
      maybeDigest = fileStream.flatMap(n => (n \\ "contentDigest").theSeq.headOption)
      _ <- maybeDigest.map(validate(bag.baseDir / s"data/$path", bag, fedoraFileId))
        .getOrElse(Success(logger.warn(s"No digest found for $fedoraFileId ${ fileStream.map(logPrinter.format(_)).getOrElse("") }")))
      fileItem <- FileItem(fedoraFileId, foXml)
    } yield fileItem
  }

  private def validate(file: File, bag: DansV0Bag, fedoraFileId: String)(maybeDigest: Node) = Try {
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
