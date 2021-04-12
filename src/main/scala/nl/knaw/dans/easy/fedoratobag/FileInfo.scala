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

import nl.knaw.dans.easy.fedoratobag.filter.InvalidTransformationException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.nio.file.{ Path, Paths }
import scala.util.{ Failure, Success, Try }
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

object FileInfo extends DebugEnhancedLogging {
  val nonAllowedCharacters = List(':', '*', '?', '"', '<', '>', '|', ';', '#')

  def replaceNonAllowedCharacters(s: String): String = {
    s.map(char => if (nonAllowedCharacters.contains(char)) '_'
                  else char)
  }

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
        Paths.get(replaceNonAllowedCharacters(get("path"))),
        replaceNonAllowedCharacters(get("name")),
        get("size").toLong,
        get("mimeType"),
        accessibleTo,
        visibleTo,
        FoXml.getStreamRoot("EASY_FILE", foXml).map(_ \\ "contentDigest").flatMap(_.headOption),
        (fileMetadata \ "additional-metadata" \ "additional" \ "content").headOption
      )
    }
  }

  /**
   * @param selectedForSecondBag will be empty if isOriginalVersioned is false
   *                             might be empty if isOriginalVersioned is true
   * @param isOriginalVersioned  true: drop original level from bagPath
   * @return Failure if at least one of the bags has files with the same bagPath
   *         otherwise fileInfosForSecondBag as first and only list if both lists have the same files
   */
  def checkDuplicates(selectedForFirstBag: Seq[FileInfo],
                      selectedForSecondBag: Seq[FileInfo],
                      isOriginalVersioned: Boolean,
                     ): Try[(Seq[FileInfo], Seq[FileInfo])] = {

    /** @return bagPath -> digestValues */
    def findDuplicateFiles(fileInfos: Seq[FileInfo]) = fileInfos
      .groupBy(_.bagPath(isOriginalVersioned))
      .filter(_._2.size > 1)
      .mapValues(infos =>
        infos.map(info =>
          s"${ info.path }[${ info.fedoraFileId },${ info.maybeDigestValue.getOrElse("") }]"
        ).mkString("[", ",", "]")
      )

    val duplicatesForFirstBag = findDuplicateFiles(selectedForFirstBag)
    val duplicatesForSecondBag = findDuplicateFiles(selectedForSecondBag)
    if (duplicatesForFirstBag.nonEmpty || duplicatesForSecondBag.nonEmpty) {
      val prefix1 = "duplicates in first bag: "
      val prefix2 = "duplicates in second bag: "
      logDuplicates(prefix1, duplicatesForFirstBag)
      logDuplicates(prefix2, duplicatesForSecondBag)
      Failure(InvalidTransformationException(
        s"$prefix1${ duplicatesForFirstBag.keys.mkString(", ") }; $prefix2${ duplicatesForSecondBag.keys.mkString(", ") } (isOriginalVersioned==$isOriginalVersioned)"
      ))
    }
    else {
      if (selectedForFirstBag.map(versionedInfo) == selectedForSecondBag.map(versionedInfo))
        Success((selectedForSecondBag, Seq.empty))
      else Success((selectedForFirstBag, selectedForSecondBag))
    }
  }

  private def versionedInfo(fileInfo: FileInfo): FileInfo = fileInfo.copy(
    path = fileInfo.bagPath(isOriginalVersioned = true),
    fedoraFileId = "",
    accessibleTo = "",
    visibleTo = "",
  )

  private def logDuplicates(prefix: String, duplicates: Map[Path, String]): Unit = {
    if (duplicates.nonEmpty)
      logger.error(prefix + duplicates.values.mkString("; "))
  }
}
