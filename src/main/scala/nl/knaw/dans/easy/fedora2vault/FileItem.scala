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

import com.typesafe.scalalogging.Logger

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node, NodeSeq, Text }

object FileItem {

  def filesXml(items: Seq[Node]): Elem =
    <files xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd"
    >
    { items }
    </files>

  def checkNotImplemented(fileItems: List[Node], logger: Logger): Try[Unit] = {
    val incompleteItems = fileItems.filter(item => (item \ "notImplemented").nonEmpty)
    incompleteItems.foreach(item =>
      (item \ "notImplemented").foreach(tag =>
        logger.warn(mockFriendly(s"${ (item \ "identifier").text } (${ item \@ "filepath" }) NOT IMPLEMENTED: ${ tag.text }"))
      )
    )

    lazy val tags = incompleteItems.flatMap(item =>
      (item \ "notImplemented").map(_.text.replaceAll(":.*", ""))
    ).distinct

    if (incompleteItems.isEmpty) Success(())
    else Failure(new Exception(s"${ incompleteItems.size } file(s) with not implemented additional file metadata: $tags"))
  }

  def apply(foXml: Node): Try[Node] = {
    FoXml.getFileMD(foXml).map { fileMetadata =>
      def get(tag: String) = {
        val strings = (fileMetadata \\ tag).map(_.text)
        if (strings.isEmpty) throw new Exception(s"<$tag> not found")
        if (strings.tail.nonEmpty) throw new Exception(s"Multiple times <$tag>")
        strings.headOption.getOrElse("")
      }

      val visibleTo = get("visibleTo")
      val accessibleTo = visibleTo.toUpperCase() match {
        case "NONE" => "NONE"
        case _ => get("accessibleTo")
      }
      <file filepath={ "data/" + get("path") }>
        <dcterms:identifier>{ foXml \@ "PID" }</dcterms:identifier>
        <dcterms:title>{ get("name") }</dcterms:title>
        <dcterms:format>{ get("mimeType") }</dcterms:format>
        <accessibleToRights>{ accessibleTo }</accessibleToRights>
        <visibleToRights>{ visibleTo }</visibleToRights>
        { (fileMetadata \ "additional-metadata" \ "additional" \ "content").flatMap(convert) }
      </file>
    }
  }

  def convert(additionalContent: Node): NodeSeq = {
    additionalContent.nonEmptyChildren.map {
      // TODO analytic_units, mapprojection, case_quantity (skip if value is one), data_format
      case Elem(_, "file_category", _, _, Text(value)) => <dcterms:type>{ value }</dcterms:type>
      case Elem(_, "original_file", _, _, Text(value)) => <dcterms:isFormatOf>{ value }</dcterms:isFormatOf>
      case Elem(_, "file_content", _, _, Text(value)) => <dcterms:abstract>{ value }</dcterms:abstract>
      case Elem(_, "file_required", _, _, Text(value)) => <dcterms:requires>{ value }</dcterms:requires>
      case Elem(_, "software", _, _, Text(value)) => <dcterms:description>{ s"This file was created with $value" }</dcterms:description>
      case Elem(_, label, _, _, Text(value)) if isNotes(label) => <dcterms:description>{ value }</dcterms:description>
      case Elem(_, "file_name", _, _, _) => Text("")
      case Elem(_, label, _, _, Text(value)) => <notImplemented>{ s"$label: $value" }</notImplemented>
      case node => node // white space
    }
  }

  private def isNotes(label: String) = {
    Seq("notes", "remarks", "file_notes", "file_remarks").contains(label)
  }
}
