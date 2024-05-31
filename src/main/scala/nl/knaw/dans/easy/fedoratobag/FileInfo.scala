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
                    wasDerivedFrom: Option[Path] = None,
                    originalPath: Path,
                    locationUrl: Option[String] = None,
                   ) {
  private val isAccessible: Boolean = accessibleTo.toUpperCase() != "NONE"
  val isOriginal: Boolean = path.startsWithOriginalFolder()
  val isAccessibleOriginal: Boolean = isOriginal && isAccessible
  val maybeDigestType: Option[String] = contentDigest.map(n => (n \\ "@TYPE").text)
  val maybeDigestValue: Option[String] = contentDigest.map(n => (n \\ "@DIGEST").text)

  def bagSource(isOriginalVersioned: Boolean): Option[Path] = wasDerivedFrom
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
      .map(parent => replaceForbiddenCharactersInPath(parent.toString) + "/")
      .getOrElse("") +
      replaceForbiddenCharactersInFileName(p.getFileName.toString)
    Paths.get(s)
  }

  def apply(fedoraIDs: Seq[String], fedoraProvider: FedoraProvider, withAv: Boolean): Try[Seq[FileInfo]] = {

    def digestValue(foXmlStream: Option[Node]): Option[Node] = foXmlStream
      .map(_ \\ "contentDigest").flatMap(_.headOption)

    def locationUrl(data: Option[Node]) = {
      val maybeNode = data.map(_ \\ "contentLocation").flatMap(_.headOption)
      val maybeLocationType = maybeNode.flatMap(_.attribute("TYPE").map(_.text))
      if (maybeLocationType.contains("URL"))
        maybeNode.flatMap(_.attribute("REF").map(_.text))
      else None
    }

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
          data = FoXml.getStreamRoot("EASY_FILE", foXml)
          digest = digestValue(data)
          path = (fileMetadata \\ "path").map(_.text).headOption.map(toValidChars)
          contentLocation = locationUrl(data)
        } yield (fileId, derivedFromId, digest, fileMetadata, path, contentLocation)
      }.map { files =>
      val pathMap = files.map {
        case (fileId, _, _, _, maybePath, _) => fileId -> maybePath
      }.toMap
      files.map {
        case (fileId, _, _, _, None, _) =>
          throw new Exception(s"<path> not found for $fileId")
        case (fileId, derivedFrom, digest, fileMetadata, Some(path), location) =>
          def get(tag: String) = (fileMetadata \\ tag)
            .map(_.text)
            .headOption
            .getOrElse(throw new Exception(s"$fileId <$tag> not found"))

          val visibleTo = get("visibleTo")
          val accessibleTo = if ("NONE" == visibleTo.toUpperCase && !withAv) "NONE"
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
            (fileMetadata \\ "path").map(_.text).headOption.map(p => Paths.get(p)).get, //when 'path' is a Some, so is this
            location,
          )
      }
    }
  }

  /**
   * @param selectedForBag2     will be empty if isOriginalVersioned is false
   *                            might be empty if isOriginalVersioned is true
   * @param isOriginalVersioned true: drop original level from bagPath
   * @return Failure if at least one of the bags has files with the same bagPath
   *         otherwise fileInfosForSecondBag as first and only list if both lists have the same files
   */
  def checkDuplicates(selectedForBag1: Seq[FileInfo],
                      selectedForBag2: Seq[FileInfo],
                      isOriginalVersioned: Boolean,
                     ): Try[(Seq[FileInfo], Seq[FileInfo])] = {

    def pickDuplicate(filesWithSameBagPath: Seq[FileInfo]): Seq[FileInfo] = {
      if (filesWithSameBagPath.size == 1)
        filesWithSameBagPath // unique is ok
      else if (filesWithSameBagPath.map(_.contentDigest).distinct.size != 1)
             filesWithSameBagPath // conflicting shas, keep both, they will be reported by caller
           else {
             // pick the the most recent file based on the id
             val latest = filesWithSameBagPath.maxBy(_.fedoraFileId)
             val msg = s"Picked ${ latest.fedoraFileId } from " + filesWithSameBagPath.mkString(", ")
             if (filesWithSameBagPath
               .map(f => (f.accessibleTo, f.visibleTo, f.additionalMetadata, f.wasDerivedFrom, f.originalPath))
               .size > 1 // the files have the same sha but different metadata
             ) logger.warn(msg)
             else logger.debug(msg)
             Seq(latest)
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
        s"Files with same bag path but different SHAs: " + conflicts.flatten.mkString(", ")
      ))
    }

    (groupByBagPath(selectedForBag1), groupByBagPath(selectedForBag2)) match {
      case (Failure(InvalidTransformationException(m1)), Failure(InvalidTransformationException(m2))) =>
        Failure(InvalidTransformationException(s"$m1 $m2"))
      case (Failure(e1), Failure(e2)) =>
        logger.error(e1.getMessage, e1)
        logger.error(e2.getMessage, e2)
        Failure(new IllegalStateException(s"Bag1: ${ e1.getMessage } Bag2: ${ e2.getMessage }"))
      case (Failure(e), _) => Failure(e)
      case (_, Failure(e)) => Failure(e)
      case (Success(forBag1), Success(forBag2)) if forBag1.map(versionedInfo) == forBag2.map(versionedInfo) =>
        Success(forBag2, Seq.empty)
      case (Success(forBag1), Success(forBag2)) =>
        Success(forBag1, forBag2)
    }
  }

  private def versionedInfo(fileInfo: FileInfo): FileInfo = fileInfo.copy(
    path = fileInfo.path.bagPath(isOriginalVersioned = true),
    originalPath = fileInfo.originalPath.bagPath(isOriginalVersioned = true),
    fedoraFileId = "",
    accessibleTo = "",
    visibleTo = "",
  )
}
