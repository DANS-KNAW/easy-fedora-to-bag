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

import com.typesafe.scalalogging.Logger

import java.nio.file.Path
import java.util.Locale
import scala.util.{ Failure, Success, Try }
import scala.xml._

object FileItem {

  private val baseNS = "http://easy.dans.knaw.nl/schemas/bag/metadata"

  def filesXml(items: Seq[Node]): Elem =
    <files xmlns:dct="http://purl.org/dc/terms/"
           xmlns:afm={ s"$baseNS/afm/"}
           xmlns={ s"$baseNS/files/" }
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation={ s"$baseNS/files/ $baseNS/files/files.xsd" }
    >
    { items }
    </files>

  def checkNotImplementedFileMetadata(fileItems: List[Node], logger: Logger): Try[Unit] = {
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

  def apply(fileInfo: FileInfo, isOriginalVersioned: Boolean): Try[Node] = Try {
    val src = fileInfo.bagSource(isOriginalVersioned).map(path =>
        <dct:source>{ "data/" + path }</dct:source>
    ).getOrElse(Seq[Node]())
    val bagPath = fileInfo.path.bagPath(isOriginalVersioned)
    val comment = if (bagPath != fileInfo.path) Comment(fileInfo.path.toString.replaceAll("--","__"))
                  else Text("")
    val originalPath = if (fileInfo.originalPath != fileInfo.path)
                         <afm:keyvaluepair><afm:key>ORIGINAL_FILE_PATH</afm:key><afm:value>{ fileInfo.originalPath.bagPath(isOriginalVersioned) }</afm:value></afm:keyvaluepair>
                       else Text("")

      <file filepath={ "data/" + bagPath }>
        { comment }
        <dct:identifier>{ fileInfo.fedoraFileId }</dct:identifier>
        <dct:title>{ fileInfo.name }</dct:title>
        <dct:format>{ fileInfo.mimeType }</dct:format>
        <dct:extent>{ "%.1fMB".formatLocal(Locale.US, fileInfo.size / 1024 / 1024) }</dct:extent>
        { fileInfo.additionalMetadata.map(convert).getOrElse( Seq[Node]()) }
        <accessibleToRights>{ fileInfo.accessibleTo }</accessibleToRights>
        <visibleToRights>{ fileInfo.visibleTo }</visibleToRights>
        { originalPath }
        { src }
      </file>
  }

  def convert(additionalContent: Node): NodeSeq = {
    val hasArchivalName = (additionalContent \ "archival_name").nonEmpty // EASY-I
    val hasOriginalFile = (additionalContent \ "original_file").nonEmpty // EASY-II
    additionalContent.nonEmptyChildren.map {
      case Elem(_, label, attributes, _, Text(value)) if attributes.nonEmpty => <notImplemented>{ s"$label(attributes: $attributes): $value" }</notImplemented>
      case Elem(_, "file_name", _, _, _) if hasArchivalName && hasOriginalFile => <notImplemented>original_file AND archival_name</notImplemented>

      case Elem(_, "original_file", _, _, Text(value)) => <dct:isFormatOf>{ value }</dct:isFormatOf>
      case Elem(_, "file_name", _, _, Text(value)) if hasOriginalFile => <dct:title>{ value }</dct:title>

      case Elem(_, "file_name", _, _, Text(value)) if hasArchivalName => <dct:isFormatOf>{ value }</dct:isFormatOf>
      case Elem(_, "archival_name", _, _, Text(value)) => <dct:title>{ value }</dct:title>

      case Elem(_, "case_quantity", _, _, Text(value)) if value.trim == "1" => Text("") // RAAP mis-interpretation
      case Elem(_, "case_quantity", _, _, Text(value)) => <afm:case_quantity>{ value }</afm:case_quantity>

      case Elem(_, "file_required", _, _, Text(value)) => <dct:requires>{ value }</dct:requires>
      case Elem(_, "file_content", _, _, Text(value)) => <dct:abstract>{ value }</dct:abstract>
      case Elem(_, label, _, _, Text(value)) if isNotes(label) => <afm:notes>{ value }</afm:notes>
      case Elem(_, label, _, _, Text(value)) if asIs(label) => <tag>{ value }</tag>.copy(prefix = "afm", label = label)
      case Elem(_, label, _, _, Text(value)) => <afm:keyvaluepair><afm:key>{ label }</afm:key><afm:value>{ value }</afm:value></afm:keyvaluepair>
      case node if node.text.isEmpty => null
      case node => node // white space
    }
  }

  private def asIs(label: String) = {
    Seq(
      "hardware", "software", "original_OS",
      "file_category", "data_format", "file_type", "othmat_codebook", "data_format",
      "data_collector", "collection_date", "time_period",
      "geog_cover", "geog_unit", "local_georef", "mapprojection",
      "analytic_units"
    ).contains(label)
  }

  private def isNotes(label: String) = {
    Seq("notes", "remarks", "file_notes", "file_remarks").contains(label)
  }
}
