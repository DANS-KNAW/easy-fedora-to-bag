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

import nl.knaw.dans.easy.fedoratobag.filter.FileFilterType._

import scala.util.{ Failure, Success, Try }
import scala.xml.Node

package object filter {
  implicit class FileInfos(val fileInfos: List[FileInfo]) extends AnyVal {
    def selectForSecondBag(isOriginalVersioned: Boolean): List[FileInfo] = {
      lazy val originals = fileInfos.filter(_.isOriginal)
      if (!isOriginalVersioned || originals.isEmpty) List.empty
      else {
        val accessibleOriginals = originals.filter(_.isAccessibleOriginal)
        if (fileInfos.size == accessibleOriginals.size) List.empty
        else accessibleOriginals ++ fileInfos.filterNot(_.isOriginal)
      }
    }

    def hasOriginalAndOthers: Boolean = {
      fileInfos.exists(_.isOriginal) && fileInfos.exists(!_.isOriginal)
    }

    def selectForFirstBag(emd: Node, hasSecondBag: Boolean, europeana: Boolean): Try[List[FileInfo]] = {

      def largest(by: FileFilterType, orElseBy: FileFilterType): Try[List[FileInfo]] = {
        val infosByType = fileInfos
          .filter(_.accessibleTo == "ANONYMOUS")
          .groupBy(fi => if (fi.mimeType.startsWith("image/")) LARGEST_IMAGE
                         else if (fi.mimeType.startsWith("application/pdf")) LARGEST_PDF
                              else ALL_FILES
          )
        val selected = infosByType.getOrElse(by, infosByType.getOrElse(orElseBy, List.empty))
        maxSizeUnlessEmpty(selected)
      }

      def maxSizeUnlessEmpty(selected: List[FileInfo]) = {
        if (selected.isEmpty) Failure(NoPayloadFilesException())
        else Success(List(selected.maxBy(_.size)))
      }

      def successUnlessEmpty(fileInfos: List[FileInfo]) = {
        if (fileInfos.isEmpty) Failure(NoPayloadFilesException())
        else Success(fileInfos)
      }

      val fileFilterType = if (hasSecondBag) ORIGINAL_FILES
                           else if (!europeana) ALL_FILES
                                else if (dcmiType(emd) == "text") LARGEST_PDF
                                     else LARGEST_IMAGE
      fileFilterType match {
        case LARGEST_PDF => largest(LARGEST_PDF, LARGEST_IMAGE)
        case LARGEST_IMAGE => largest(LARGEST_IMAGE, LARGEST_PDF)
        case ORIGINAL_FILES => successUnlessEmpty(fileInfos.filter(_.isOriginal)) // TODO is ALL_FILES if no second bag
        case ALL_FILES => successUnlessEmpty(fileInfos)
      }
    }
  }

  private def dcmiType(emd: Node): String = {
    def hasDcmiScheme(node: Node) = node
      .attribute("http://easy.dans.knaw.nl/easy/easymetadata/eas/", "scheme")
      .exists(_.text == "DCMI")

    (emd \ "type" \ "type")
      .filter(hasDcmiScheme)
      .text.toLowerCase.trim
  }
}
