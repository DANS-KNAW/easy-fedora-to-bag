package nl.knaw.dans.easy.fedora2vault

import scala.util.Try
import scala.xml.{ Elem, Node }

case class FileItem(fedoraFileId: String, sha: String, xml: Node) {
  val path: String = (xml \ "@filepath").text
}

object FileItem {
  def assemble(items: Seq[FileItem]): Elem =
    <files xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd"
    >
      { items.map(_.xml) }
    </files>

  def apply(fedoraFileId: String, foXml: Node): Try[FileItem] = Try {
    val streamId = "EASY_FILE_METADATA"
    val fileMetadata = FoXml.getStreamRoot(streamId, foXml)
      .getOrElse(throw new Exception(s"no $streamId for $fedoraFileId"))

    def get(tag: DatasetId) = {
      val strings = (fileMetadata \\ tag).map(_.text)
      if(strings.isEmpty)
        throw new Exception(s"No <$tag> in $streamId for $fedoraFileId")
      if(strings.tail.nonEmpty)
        throw new Exception(s"Multiple times <$tag> in $streamId for $fedoraFileId")
      strings.headOption.getOrElse("")
    }

    // note that the contentDigest is found in different streams
    // such as streamId: EASY_FILE and EASY_FILE_METADATA
    new FileItem(
      fedoraFileId,
      (foXml \ "datastream" \ "datastreamVersion" \ "contentDigest" \ "@DIGEST").text,
      <file filepath={ "data/" + get("path") }>
        <dcterms:title>{ get("name") }</dcterms:title>
        <dcterms:format>{ get("mimeType") }</dcterms:format>
        <accessibleToRights>{ get("accessibleTo") }</accessibleToRights>
        <visibleToRights>{ get("visibleTo") }</visibleToRights>
      </file>
    )
  }
}
