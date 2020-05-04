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

import java.io.InputStream
import java.nio.file.{ Path, Paths }
import java.util.UUID

import better.files.{ File, StringExtensions }
import com.yourmediashelf.fedora.client.FedoraClient
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedora2vault.Command.FeedBackMessage
import nl.knaw.dans.easy.fedora2vault.FoXml.{ getEmd, _ }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.joda.time.DateTime

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node }

class EasyFedora2vaultApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  private lazy val ldap = new Ldap(ldapContext)
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def simpleTransForms(input: File, outputDir: File): Try[FeedBackMessage] = {
    input.lineIterator.filterNot(_.startsWith("#")).map(datasetId => {
      simpleTransform(datasetId, outputDir)
        .doIfFailure { case t =>
          logger.error(s"$datasetId failed", t)
        }
    }).collectFirst { case t @ Failure(_) => t }
      .getOrElse(Success(s"All datasets in ${ input } saved as bags in ${ outputDir }"))
    // TODO return individual feedback messages preceded with FeedBack.header
  }

  def simpleTransform(datasetId: DatasetId, outputDir: File): Try[FeedBackMessage] = {

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

    def compareManifest(bag: DansV0Bag)(streamId: String): Try[Any] = {
      fedoraProvider.disseminateDatastream(datasetId, streamId)
        .map { inputStream: InputStream =>
          val manifests = bag.payloadManifests
          Success(()) // TODO EASY-2678
        }.tried.flatten
    }

    val uuid = UUID.randomUUID()
    for {
      foXml <- fedoraProvider.loadFoXml(datasetId)
      depositor <- getOwner(foXml)
      msg = s"$outputDir from $datasetId with owner $depositor"
      _ = logger.info("Created " + msg)
      bag <- DansV0Bag.empty(outputDir / uuid.toString)
        .map(_.withEasyUserAccount(depositor).withCreated(DateTime.now()))
      emdXml <- getEmd(foXml)
      _ <- addXmlMetadata(bag, "emd.xml")(emdXml)
      amd <- getAmd(foXml)
      _ <- addXmlMetadata(bag, "amd.xml")(amd)
      _ <- getDdm(foXml)
        .map(addXmlPayload(bag, "original-ddm.xml"))
        .getOrElse(Success())
      emd <- Try(emdUnmarshaller.unmarshal(emdXml.serialize))
      audiences <- emd.getEmdAudience.getDisciplines.asScala
        .map(id => getAudience(id.getValue)).collectResults
      ddm <- DDM(emd, audiences)
      _ <- addXmlMetadata(bag, "dataset.xml")(ddm)
      _ <- getMessageFromDepositor(foXml)
        .map(addXmlMetadata(bag, "depositor-info/message-from-depositor.txt"))
        .getOrElse(Success(())) // TODO EASY-2697: EMD/other/remark
      _ <- getFilesXml(foXml)
        .map(addXmlMetadata(bag, "files.xml"))
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
      _ <- fedoraIDs.toStream
        .withFilter(_.startsWith("easy-file:"))
        .map(addPayloadFileTo(bag)).failFastOr(Success(()))
      _ <- bag.save()
      _ <- getManifest(foXml)
        .map(compareManifest(bag))
        .getOrElse(Success(())) // TODO check with sha's from fedora
      doi = emd.getEmdIdentifier.getDansManagedDoi
    } yield FeedBack(datasetId, doi, depositor, TransformationType.SIMPLE, uuid, "???").toString
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
    bag.addTagFile(content.serialize.inputStream, Paths.get(path))
  }

  private def addPayloadFileTo(bag: DansV0Bag)(fedoraFileId: String): Try[Path] = {
    fedoraProvider.loadFoXml(fedoraFileId)
      .flatMap { foXml =>
        val path = Paths.get((foXml \\ "file-item-md" \\ "path").text)
        logger.info(s"Adding $fedoraFileId to $path")
        fedoraProvider
          .disseminateDatastream(fedoraFileId, "EASY_FILE")
          .map(bag.addPayloadFile(_, path))
          .tried.flatten
          .map(_ => path)
      }
  }
}
