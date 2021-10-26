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
  val isOriginal: Boolean = path.startsWithOriginalFolder()
  val isAccessibleOriginal: Boolean = isOriginal && isAccessible
  val maybeDigestType: Option[String] = contentDigest.map(n => (n \\ "@TYPE").text)
  val maybeDigestValue: Option[String] = contentDigest.map(n => (n \\ "@DIGEST").text)

  def bagSource(isOriginalVersioned: Boolean): Option[Path] = wasDerivedForm
    .map(_.path.bagPath(isOriginalVersioned))
}

object FileInfo extends DebugEnhancedLogging {
  //private val allowedCharactersInPath =(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('_', '-', '.', '\\', '/', ' ')).toSet
  private val forbiddenCharactersInFileName = List(':', '*', '?', '"', '<', '>', '|', ';', '#')
  private val forbiddenCharactersInPathName = forbiddenCharactersInFileName ++ List('(', ')', ',', '\'', '[', ']', '&', '+')

  private def replaceForbiddenCharactersInPath(s: String): String = {
    s.map(char => if (forbiddenCharactersInPathName.contains(char)) '_'
                  else char)
  }
  private def replaceForbiddenCharactersInFileName(s: String): String = {
    s.map(char => if (forbiddenCharactersInFileName.contains(char)) '_'
                  else char)
  }

  private def toValidChars(fileMetadataPath: String) = {
    val p = Paths.get(fileMetadataPath)
    val s = Option(p.getParent)
      .map(parent => replaceForbiddenCharactersInPath(parent.toString)+"/")
      .getOrElse("") +
      replaceForbiddenCharactersInFileName(p.getFileName.toString)
    Paths.get(s)
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
          path = (fileMetadata \\ "path").map(_.text).headOption.map(toValidChars)
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
            replaceForbiddenCharactersInFileName(get("name")),
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

    def pickDuplicate(filesWithSameBagPath: Seq[FileInfo]): Seq[FileInfo] = {
      if (filesWithSameBagPath.size == 1)
        filesWithSameBagPath // unique is ok
      else if (filesWithSameBagPath.map(_.contentDigest).distinct.size != 1)
             filesWithSameBagPath // conflicting shas, keep both, they will be reported by caller
           else {
             // minBy avoids the original folder
             logger.warn(s"Picked the shortest path from " + filesWithSameBagPath.mkString(", "))
             Seq(filesWithSameBagPath.minBy(_.path.toString.length))
             // TODO pick the least access or richest metadata
           }
    }

    def groupByBagPath(fileInfos: Seq[FileInfo]): Try[Seq[FileInfo]] = {
      val grouped = fileInfos
        .groupBy(_.path.bagPath(isOriginalVersioned))
        .values
        .map(pickDuplicate).toSeq
      val conflicts = grouped.filter(_.size > 1)
      if (conflicts.isEmpty) Success(grouped.flatten)
      else Failure(InvalidTransformationException(
        s"Files with same bag path but different shas: " + conflicts.flatten.mkString(", ")
      ))
    }

    for {
      // TODO conflicting duplicate paths for second bag are not report if first one has too
      forFirst <- groupByBagPath(selectedForFirstBag)
      forSecond <- groupByBagPath(selectedForSecondBag)
    } yield if (forFirst.map(versionedInfo) == forSecond.map(versionedInfo))
              (forSecond, Seq.empty)
            else (forFirst, forSecond)
  }

  private def versionedInfo(fileInfo: FileInfo): FileInfo = fileInfo.copy(
    path = fileInfo.path.bagPath(isOriginalVersioned = true),
    originalPath = fileInfo.originalPath.bagPath(isOriginalVersioned = true),
    fedoraFileId = "",
    accessibleTo = "",
    visibleTo = "",
  )
}
