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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try
import scala.xml.Node

object FoXml extends DebugEnhancedLogging {

  private def getStream(streamId: String, rootTag: String, foXml: Node): Try[Node] = {
    Try {
      val node = getStreamRoot(streamId, foXml)
        .filter(hasControlGroup("X"))
        .getOrElse(throw new Exception(s"Stream with ID=$streamId and CONTROL_GROUP=X not found"))

      (node \\ "xmlContent")
        .last
        .descendant
        .filter(_.label == rootTag)
        .last
    }
  }

  def getStreamRoot(streamId: String, foXml: Node): Option[Node] = {
    trace(streamId)
    (foXml \ "datastream")
      .theSeq
      .filter(n => n \@ "ID" == streamId)
      .lastOption
  }

  def getEmd(foXml: Node): Try[Node] = getStream("EMD", "easymetadata", foXml)

  def getAmd(foXml: Node): Try[Node] = getStream("AMD", "administrative-md", foXml)

  def getRelsExt(foXml: Node): Try[Node] = getStream("RELS-EXT", "RDF", foXml)

  // TODO EASY_ITEM_CONTAINER_MD in http://deasy.dans.knaw.nl:8080/fedora/objects/easy-dataset:10/objectXML

  def getDdm(foXml: Node): Option[Node] = getStream("dataset.xml", "DDM", foXml).toOption

  def getFilesXml(foXml: Node): Option[Node] = getStream("files.xml", "file", foXml).toOption

  def getFileMD(fileFoXml: Node): Try[Node] = getStream("EASY_FILE_METADATA", "file-item-md", fileFoXml)

  def getAgreementsXml(foXml: Node): Option[Node] = getStream("agreements.xml", "agreements", foXml).toOption

  def getManifest(foXml: Node): Option[String] = {
    val streamId = "manifest-sha1.txt"
    getStreamRoot(streamId, foXml)
      .map(_ => streamId)
  }

  def getMessageFromDepositor(foXml: Node): Option[Node] = getStreamRoot("message-from-depositor.txt", foXml)

  def managedStreamLabel(foXml: Node, id: String): Option[String] = {
    getStreamRoot(id, foXml)
      .withFilter(hasControlGroup("M"))
      .flatMap(getLabel)
  }

  private def hasControlGroup(controlGroup: String)(streamRoot: Node): Boolean = {
    streamRoot.attribute("CONTROL_GROUP").map(_.text).contains(controlGroup)
  }

  def getLabel(streamRoot: Node): Option[String] = {
    (streamRoot \ "datastreamVersion" \ "@LABEL").headOption.map(_.text)
  }

  def getOwner(foXml: Node): Try[Depositor] = Try {
    (foXml \ "objectProperties" \ "property")
      .filter(prop => (prop \ "@NAME").text.endsWith("#ownerId"))
      .flatMap(_ \ "@VALUE")
      .flatten.headOption.map(_.text)
      .getOrElse(throw new Exception("""FoXml has no <foxml:property NAME="info:fedora/fedora-system:def/model#ownerId" VALUE="???"/>"""))
  }

  def getEmdLicenseAccepted(foXml: Node): Try[String] = Try {
    (foXml \\ "easymetadata" \ "rights" \ "license").text
  }

  def getEmdDateSubmitted(foXml: Node): Try[String] = Try {
    (foXml \\ "easymetadata" \\ "dateSubmitted").text
  }
}
