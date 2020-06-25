package nl.knaw.dans.easy.fedora2vault

import nl.knaw.dans.easy.fedora2vault.fixture.TestSupportFixture

import scala.util.{ Success, Try }
import scala.xml.XML

class FilesItemSpec extends TestSupportFixture {
  "apply" should "" in {
    val foXml = Try{
      XML.loadFile("src/test/resources/sample-foxml/easy-file-35.xml")
    }.getOrElse(fail("could not load test data"))

    FileItem("easy-file:35", foXml) shouldBe Success (FileItem(
      <file filepath="data/original/something.txt">
        <dcterms:identifier>easy-file:35</dcterms:identifier>
        <dcterms:title>something.txt</dcterms:title>
        <dcterms:format>text/plain</dcterms:format>
        <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
        <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
  }
}
