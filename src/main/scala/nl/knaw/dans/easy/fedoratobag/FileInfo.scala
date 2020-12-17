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

import java.nio.file.{ Path, Paths }

import scala.util.Try
import scala.xml.Node

case class FileInfo(fedoraFileId: String,
                    path: Path,
                    name: String,
                    size: Double,
                    mimeType: String,
                    accessibleTo: String,
                    visibleTo: String,
                    contentDigest: Option[Node],
                    additionalMetadata: Option[Node],
                   ) {
  private val isAccessible: Boolean = accessibleTo.toUpperCase() != "NONE"
  val isOriginal: Boolean = path.getName(0).toString.toLowerCase == "original"
  val isAccessibleOriginal: Boolean = isOriginal && isAccessible
  val maybeDigestType: Option[String] = contentDigest.map(n => (n \\ "@TYPE").text)
  val maybeDigestValue: Option[String] = contentDigest.map(n => (n \\ "@DIGEST").text)

  def bagPath(isOriginalVersioned: Boolean): Path =
    if (isOriginalVersioned && isOriginal) path.subpath(1, path.getNameCount)
    else path
}

object FileInfo {
  def apply(foXml: Node): Try[FileInfo] = {
    FoXml.getFileMD(foXml).map { fileMetadata =>
      def get(tag: String) = (fileMetadata \\ tag)
        .map(_.text)
        .headOption
        .getOrElse(throw new Exception(s"<$tag> not found"))

      val visibleTo = get("visibleTo")
      val accessibleTo = visibleTo.toUpperCase() match {
        case "NONE" => "NONE"
        case _ => get("accessibleTo")
      }

      new FileInfo(
        foXml \@ "PID",
        Paths.get(get("path")),
        get("name"),
        get("size").toLong,
        get("mimeType"),
        accessibleTo,
        visibleTo,
        FoXml.getStreamRoot("EASY_FILE", foXml).map(_ \\ "contentDigest").flatMap(_.headOption),
        (fileMetadata \ "additional-metadata" \ "additional" \ "content").headOption
      )
    }
  }
}
