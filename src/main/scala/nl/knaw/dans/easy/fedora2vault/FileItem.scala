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

import scala.util.Try
import scala.xml.{ Elem, Node }

case class FileItem(xml: Node)

object FileItem {

  def filesXml(items: Seq[FileItem]): Elem =
    <files xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd"
    >
    { items.map(_.xml) }
    </files>

  def apply(fedoraFileId: String, foXml: Node): Try[FileItem] = Try {
    val streamId = "EASY_FILE_METADATA"
    val fileMetadata = FoXml.getStreamRoot(streamId, foXml)
      .getOrElse(throw new Exception(s"No $streamId for $fedoraFileId"))

    def get(tag: DatasetId) = {
      val strings = (fileMetadata \\ tag).map(_.text)
      if (strings.isEmpty)
        throw new Exception(s"No <$tag> in $streamId for $fedoraFileId")
      if (strings.tail.nonEmpty)
        throw new Exception(s"Multiple times <$tag> in $streamId for $fedoraFileId")
      strings.headOption.getOrElse("")
    }

    new FileItem(
      <file filepath={ "data/" + get("path") }>
        <dcterms:title>{ get("name") }</dcterms:title>
        <dcterms:format>{ get("mimeType") }</dcterms:format>
        <accessibleToRights>{ get("accessibleTo") }</accessibleToRights>
        <visibleToRights>{ get("visibleTo") }</visibleToRights>
      </file>
    )
  }
}
