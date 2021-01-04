package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.easy.fedoratobag.filter.FileFilterType._

import scala.util.{ Failure, Success, Try }
import scala.xml.Node

package object filter {
  implicit class FileInfos(val fileInfos: List[FileInfo]) extends AnyVal {
    def selectForSecondBag(isOriginalVersioned: Boolean): List[FileInfo] = {
      if (!isOriginalVersioned) List.empty
      else {
        val accessibleOriginals = fileInfos.filter(_.isAccessibleOriginal)
        if (fileInfos.size == accessibleOriginals.size) List.empty
        else accessibleOriginals ++ fileInfos.filterNot(_.isOriginal)
      }
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
