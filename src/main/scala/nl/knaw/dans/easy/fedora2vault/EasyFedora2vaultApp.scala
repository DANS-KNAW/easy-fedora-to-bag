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
import java.nio.file.Paths

import better.files.{ File, StringExtensions }
import com.yourmediashelf.fedora.client.FedoraClient
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedora2vault.Command.FeedBackMessage
import nl.knaw.dans.easy.fedora2vault.FoXml.{ getEmd, _ }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }
import scala.xml.{ Elem, Node }

class EasyFedora2vaultApp(configuration: Configuration) extends DebugEnhancedLogging {
  lazy val fedoraProvider: FedoraProvider = new FedoraProvider(new FedoraClient(configuration.fedoraCredentials))
  lazy val ldapContext: InitialLdapContext = new InitialLdapContext(configuration.ldapEnv, null)
  private lazy val ldap = new Ldap(ldapContext)

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

    for {
      foXml <- fedoraProvider.loadFoXml(datasetId)
      depositor <- getOwner(foXml)
      bag <- DansV0Bag.empty(outputDir).map(_.withEasyUserAccount(depositor))
      _ <- getEmd(foXml)
        .flatMap(addMetadataXml(bag, "emd.xml"))
      _ <- getAmd(foXml)
        .flatMap(addMetadataXml(bag, "amd.xml"))
      _ <- getDdm(foXml)
        .map(addMetadataXml(bag, "dataset.xml"))
        .getOrElse(Success(())) // TODO EASY-2683 EMD -> DDM
      _ <- getMessageFromDepositor(foXml)
        .map(addMetadataXml(bag, "depositor-info/message-from-depositor.txt"))
        .getOrElse(Success(())) // TODO EASY-2697: EMD/other/remark
      _ <- getFilesXml(foXml)
        .map(addMetadataXml(bag, "files.xml"))
        .getOrElse(Success(()))
      _ <- getAgreementsXml(foXml)
        .map(addAgreements(bag))
        .getOrElse(AgreementsXml(foXml, ldap)
          .map(addAgreements(bag)))
      _ <- managedMetadataStream(foXml, "ADDITIONAL_LICENSE", bag, "license") // TODO EASY-2696 where to put?
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
    } yield "???" // TODO what?
  }

  private def addMetadataStream(bag: DansV0Bag, target: String)(content: InputStream): Try[Any] = {
    bag.addTagFile(content, Paths.get(s"metadata/$target"))
  }

  private def addMetadataXml(bag: DansV0Bag, target: String)(content: Node): Try[Any] = {
    bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/$target"))
  }

  private def addAgreements(bag: DansV0Bag)(content: Node): Try[Any] = {
    bag.addTagFile(content.serialize.inputStream, Paths.get(s"metadata/depositor-info/agreements.xml"))
  }

  private def addPayloadFileTo(bag: DansV0Bag)(fedoraFileId: String): Try[DansV0Bag] = {
    fedoraProvider.loadFoXml(fedoraFileId)
      .flatMap(foXml => fedoraProvider
        .disseminateDatastream(fedoraFileId, "EASY_FILE")
        .map(bag.addPayloadFile(_, Paths.get((foXml \\ "file-item-md" \\ "path").text)))
        .tried.flatten
      )
  }
}
