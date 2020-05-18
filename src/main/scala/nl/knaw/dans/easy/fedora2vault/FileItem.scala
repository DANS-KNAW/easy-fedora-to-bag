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

import nl.knaw.dans.bag.ChecksumAlgorithm
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedora2vault.FileItem.algorithms

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node }

case class FileItem(fedoraFileId: String,
                    digestValue: String,
                    digestType: String,
                    xml: Node
                   ) {

  val file: String = (xml \ "@filepath").text

  def validateChecksum(bag: DansV0Bag): Try[Unit] = {
    val maybeChecksum = for {
      algorithm <- algorithms.get(digestType)
      manifest <- bag.payloadManifests.get(algorithm)
      checksum <- manifest.get(bag.baseDir / file)
    } yield checksum
    maybeChecksum.map(compareSha)
      .getOrElse(Failure(new Exception(s"Could not find $digestType for $fedoraFileId $file in manifest")))
  }

  private def compareSha(sha: String) = {
    if (sha == digestValue) Success(())
    else Failure(new Exception(s"checksum error fedora[$digestValue] bag[$sha] $fedoraFileId $file"))
  }
}

object FileItem {
  private val algorithms = Map("SHA-1" -> ChecksumAlgorithm.SHA1)

  def filesXml(items: Seq[FileItem]): Elem =
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
      .getOrElse(throw new Exception(s"No $streamId for $fedoraFileId"))

    def get(tag: DatasetId) = {
      val strings = (fileMetadata \\ tag).map(_.text)
      if (strings.isEmpty)
        throw new Exception(s"No <$tag> in $streamId for $fedoraFileId")
      if (strings.tail.nonEmpty)
        throw new Exception(s"Multiple times <$tag> in $streamId for $fedoraFileId")
      strings.headOption.getOrElse("")
    }

    // note that the contentDigest is found in different streams
    // such as streamId: EASY_FILE and EASY_FILE_METADATA
    val digest = foXml \ "datastream" \ "datastreamVersion" \ "contentDigest"
    new FileItem(
      fedoraFileId,
      (digest \ "@DIGEST").text,
      (digest \ "@TYPE").text,
      <file filepath={ "data/" + get("path") }>
        <dcterms:title>{ get("name") }</dcterms:title>
        <dcterms:format>{ get("mimeType") }</dcterms:format>
        <accessibleToRights>{ get("accessibleTo") }</accessibleToRights>
        <visibleToRights>{ get("visibleTo") }</visibleToRights>
      </file>
    )
  }
}
