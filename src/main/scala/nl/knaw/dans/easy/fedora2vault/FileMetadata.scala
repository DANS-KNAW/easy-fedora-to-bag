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

import scala.xml.{ Elem, NodeSeq }

object FileMetadata {
  def apply(nodes: Seq[NodeSeq]): Elem = {
    <files xmlns:dcterms="http://purl.org/dc/terms/" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd">
      {
        nodes.map(fileItemMd =>
          <file filepath={ "data/" + (fileItemMd \ "path").text }>
            <dcterms:title>{ (fileItemMd \ "name").text }</dcterms:title>
            <dcterms:format>{ (fileItemMd \ "mimeType").text }</dcterms:format>
            <dcterms:created>{ (fileItemMd \ "created").text }</dcterms:created>
            <accessibleToRights>{ (fileItemMd \ "visibleTo").text }</accessibleToRights>
            <visibleToRights>{ (fileItemMd \ "accessibleTo").text }</visibleToRights>
          </file>
        )
      }
    </files>
  }
}
