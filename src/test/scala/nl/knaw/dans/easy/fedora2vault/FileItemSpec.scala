package nl.knaw.dans.easy.fedora2vault

import nl.knaw.dans.easy.fedora2vault.fixture.TestSupportFixture

import scala.util.{ Failure, Success }
import scala.xml.NodeBuffer

class FileItemSpec extends TestSupportFixture {
  "apply" should "copy both types of rights" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <size>30</size>
                       <creatorRole>DEPOSITOR</creatorRole>
                       <visibleTo>ANONYMOUS</visibleTo>
                       <accessibleTo>RESTRICTED_REQUEST</accessibleTo>

    FileItem(fileFoXml(fileMetadata)) shouldBe Success(
    <file filepath="data/original/something.txt">
      <dcterms:identifier>easy-file:35</dcterms:identifier>
      <dcterms:title>something.txt</dcterms:title>
      <dcterms:format>text/plain</dcterms:format>
      <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
      <visibleToRights>ANONYMOUS</visibleToRights>
    </file>
    )
  }

  it should "use a default for accessibleTo" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <visibleTo>NONE</visibleTo>

    FileItem(fileFoXml(fileMetadata)) shouldBe Success(
    <file filepath="data/original/something.txt">
      <dcterms:identifier>easy-file:35</dcterms:identifier>
      <dcterms:title>something.txt</dcterms:title>
      <dcterms:format>text/plain</dcterms:format>
      <accessibleToRights>NONE</accessibleToRights>
      <visibleToRights>NONE</visibleToRights>
    </file>
    )
  }

  it should "report a repeated tag" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <name>blabla</name>
                       <visibleTo>NONE</visibleTo>

    FileItem(fileFoXml(fileMetadata)) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        "Multiple times <name> in EASY_FILE_METADATA for easy-file:35" =>
    }
  }

  it should "report a missing tag" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <visibleTo>NONE</visibleTo>

    FileItem(fileFoXml(fileMetadata)) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        "No <mimeType> in EASY_FILE_METADATA for easy-file:35" =>
    }
  }

  private def fileFoXml(fileMetadata: NodeBuffer) = {
      <foxml:digitalObject VERSION="1.1" PID="easy-file:35"
             xmlns:foxml="info:fedora/fedora-system:def/foxml#"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd">
          <foxml:datastream ID="EASY_FILE_METADATA" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
              <foxml:datastreamVersion ID="EASY_FILE_METADATA.0" LABEL="" CREATED="2020-03-17T10:24:17.660Z" MIMETYPE="text/xml" SIZE="359">
                  <foxml:xmlContent>
                      <fimd:file-item-md xmlns:fimd="http://easy.dans.knaw.nl/easy/file-item-md/" version="0.1">
                          { fileMetadata }
                      </fimd:file-item-md>
                  </foxml:xmlContent>
              </foxml:datastreamVersion>
          </foxml:datastream>
      </foxml:digitalObject>
  }
}
