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

import cats.implicits.catsStdInstancesForTry
import cats.instances.list._
import cats.syntax.traverse._
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
                    wasDerivedForm: Option[Path] = None,
                    originalPath: Path,
                   ) {
  private val isAccessible: Boolean = accessibleTo.toUpperCase() != "NONE"
  val isOriginal: Boolean = path.startsWithOriginalFolder
  val isAccessibleOriginal: Boolean = isOriginal && isAccessible
  val maybeDigestType: Option[String] = contentDigest.map(n => (n \\ "@TYPE").text)
  val maybeDigestValue: Option[String] = contentDigest.map(n => (n \\ "@DIGEST").text)

  def bagSource(isOriginalVersioned: Boolean): Option[Path] = wasDerivedForm
    .map(_.path.bagPath(isOriginalVersioned))
}

object FileInfo extends DebugEnhancedLogging {
  //private val allowedCharactersInPath =(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('_', '-', '.', '\\', '/', ' ')).toSet
  private val nonAllowedCharactersInFileName = List(':', '*', '?', '"', '<', '>', '|', ';', '#')
  private val nonAllowedCharactersInPathName = nonAllowedCharactersInFileName ++ List('(', ')', ',', '\'', '[', ']', '&', '+')

  private def replaceNonAllowedCharactersInPath(s: String): String = {
    s.map(char => if (nonAllowedCharactersInPathName.contains(char)) '_'
                  else char)
  }
  private def replaceNonAllowedCharactersInFileName(s: String): String = {
    s.map(char => if (nonAllowedCharactersInFileName.contains(char)) '_'
                  else char)
  }

  def apply(fedoraIDs: Seq[String], fedoraProvider: FedoraProvider): Try[Seq[FileInfo]] = {

    def digestValue(foXmlStream: Option[Node]): Option[Node] = foXmlStream
      .map(_ \\ "contentDigest").flatMap(_.headOption)

    def derivedFrom(foXmlStream: Option[Node]): Option[String] = {
      foXmlStream
        .flatMap(n => (n \\ "wasDerivedFrom").headOption).toSeq
        .flatMap(node =>
          node.attribute("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "resource")
            .toSeq.flatten
            .map(_.text.replace("info:fedora/", ""))
        ).headOption
    }

    fedoraIDs
      .filter(_.startsWith("easy-file:")).toList
      .traverse { fileId =>
        for {
          foXml <- fedoraProvider.loadFoXml(fileId)
          fileMetadata <- FoXml.getFileMD(foXml)
          derivedFromId = derivedFrom(FoXml.getStreamRoot("RELS-EXT", foXml))
          digest = digestValue(FoXml.getStreamRoot("EASY_FILE", foXml))
          path = (fileMetadata \\ "path").map(_.text).headOption
            .map(p => Paths.get(replaceNonAllowedCharactersInPath(p)))
        } yield (fileId, derivedFromId, digest, fileMetadata, path)
      }.map { files =>
      val pathMap = files.map {
        case (fileId, _, _, _, maybePath) => fileId -> maybePath}.toMap
      files.map {
        case (fileId, _, _, _, None) =>
          throw new Exception(s"<path> not found for $fileId")
        case (fileId, derivedFrom, digest, fileMetadata, Some(path)) =>
          def get(tag: String) = (fileMetadata \\ tag)
            .map(_.text)
            .headOption
            .getOrElse(throw new Exception(s"$fileId <$tag> not found"))

          val visibleTo = get("visibleTo")
          val accessibleTo = if ("NONE" == visibleTo.toUpperCase) "NONE"
                             else get("accessibleTo")
          new FileInfo(
            fileId, path,
            replaceNonAllowedCharactersInFileName(get("name")),
            get("size").toLong,
            get("mimeType"),
            accessibleTo,
            visibleTo,
            digest,
            (fileMetadata \ "additional-metadata" \ "additional" \ "content").headOption,
            derivedFrom.flatMap(pathMap), // TODO error handling
            (fileMetadata \\ "path").map(_.text).headOption.map(p=>Paths.get(p)).get,  //when 'path' is a Some, so is this
          )
      }
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
      .groupBy(_.path.bagPath(isOriginalVersioned))
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
    path = fileInfo.path.bagPath(isOriginalVersioned = true),
    originalPath = fileInfo.originalPath.bagPath(isOriginalVersioned = true),
    fedoraFileId = "",
    accessibleTo = "",
    visibleTo = "",
    originalPath = fileInfo.bagPath(isOriginalVersioned = true),
  )

  private def logDuplicates(prefix: String, duplicates: Map[Path, String]): Unit = {
    if (duplicates.nonEmpty)
      logger.error(prefix + duplicates.values.mkString("; "))
  }
}
