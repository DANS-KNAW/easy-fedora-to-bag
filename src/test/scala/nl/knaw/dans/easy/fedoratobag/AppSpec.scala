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

import java.util.UUID

import better.files.{ File, _ }
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedoratobag.FileFilterType._
import nl.knaw.dans.easy.fedoratobag.filter.{ BagIndex, DatasetFilter, InvalidTransformationException, SimpleDatasetFilter }
import nl.knaw.dans.easy.fedoratobag.fixture._
import org.scalamock.scalatest.MockFactory
import resource.managed

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, XML }

class AppSpec extends TestSupportFixture with FileFoXmlSupport with BagIndexSupport with MockFactory with FileSystemSupport with AudienceSupport {
  implicit val logFile: File = testDir / "log.txt"

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

  private class MockedApp(configuration: Configuration = new Configuration("test-version", null, null, null, null, AbrMappings(File("src/main/assembly/dist/cfg/EMD_acdm.xsl"))),
                         ) extends EasyFedoraToBagApp(configuration) {
    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
    override lazy val bagIndex: BagIndex = mockBagIndexRespondsWith(body = "<result/>", code = 200)
    val filter: SimpleDatasetFilter = SimpleDatasetFilter(bagIndex)

    // make almost private methods available for tests

    override def createBag(datasetId: DatasetId, bagDir: File, strict: Boolean, europeana: Boolean, datasetFilter: DatasetFilter): Try[CsvRecord] =
      super.createBag(datasetId, bagDir, strict, europeana, datasetFilter)

    override def addPayloads(bag: DansV0Bag, fileFilterType: FileFilterType, fileIds: Seq[String]): Try[List[Node]] =
      super.addPayloads(bag, fileFilterType, fileIds)
  }

  "createBag" should "process DepositApi" in {
    val app = new MockedApp()
    Map(
      "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
      "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
      "easy-file:35" -> fileFoXml(),
    ).foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }
    Seq(
      ("easy-dataset:17", "ADDITIONAL_LICENSE", "lalala"),
      ("easy-dataset:17", "DATASET_LICENSE", "blablabla"),
      ("easy-file:35", "EASY_FILE", "acabadabra"),
    ).foreach { case (objectId, streamId, content) =>
      (app.fedoraProvider.disseminateDatastream(_: String, _: String)) expects(objectId, streamId
      ) once() returning managed(content.inputStream)
    }
    (app.fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
      Success(Seq("easy-file:35"))

    // end of mocking

    val uuid = UUID.randomUUID
    app.createBag("easy-dataset:17", testDir / "bags" / uuid.toString, strict = true, europeana = false, app.filter) shouldBe
      Success(CsvRecord("easy-dataset:17", uuid, "10.17026/test-Iiib-z9p-4ywa", "user001", "simple", "OK"))

    // post conditions

    val metadata = (testDir / "bags").children.next() / "metadata"
    (metadata / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (metadata / "license.pdf").contentAsString shouldBe "lalala"
    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf", "original")
    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf", "message-from-depositor.txt")
  }

  it should "report not strict simple violation" in {
    val app = new MockedApp()
    (app.fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
      Success(Seq("easy-jumpoff:1"))
    Map(
      "ADDITIONAL_LICENSE" -> "lalala",
      "DATASET_LICENSE" -> "blablabla",
    ).foreach { case (streamId, content) =>
      (app.fedoraProvider.disseminateDatastream(_: String, _: String)) expects("easy-dataset:17", streamId
      ) once() returning managed(content.inputStream)
    }
    Map(
      "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
      "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
    ).foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }

    // end of mocking

    val uuid = UUID.randomUUID
    val bagDir = testDir / "bags" / uuid.toString
    app.createBag("easy-dataset:17", bagDir, strict = false, europeana = false, app.filter) shouldBe
      Failure(NoPayloadFilesException())

    // post conditions

    val metadata = (testDir / "bags").children.next() / "metadata"
    (metadata / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (metadata / "license.pdf").contentAsString shouldBe "lalala"
    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "license.pdf", "original")
    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf", "message-from-depositor.txt")
  }

  it should "report strict simple violation" in {
    val app = new MockedApp()
    (app.fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
      Success(Seq("easy-jumpoff:1"))
    Map(
      "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
      "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
    ).foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }

    // end of mocking

    val uuid = UUID.randomUUID
    val bagDir = testDir / "bags" / uuid.toString
    app.createBag("easy-dataset:17", bagDir, strict = true, europeana = false, app.filter) should matchPattern {
      case Failure(_: InvalidTransformationException) =>
    }
    (testDir / "bags") shouldNot exist
  }

  it should "process streaming" in {
    val app = new MockedApp()
    expectAUser(app.ldapContext)
    Map(
      "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
      "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
      "easy-file:35" -> fileFoXml(),
    ).foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }
    (app.fedoraProvider.disseminateDatastream(_: String, _: String)) expects(
      "easy-file:35",
      "EASY_FILE"
    ) once() returning managed("barbapapa".inputStream)
    (app.fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
      Success(Seq("easy-file:35"))

    // end of mocking

    val uuid = UUID.randomUUID
    val bagDir = testDir / "bags" / uuid.toString
    app.createBag("easy-dataset:13", bagDir, strict = true, europeana = false, app.filter) shouldBe
      Success(CsvRecord("easy-dataset:13", uuid, "10.17026/mocked-Iiib-z9p-4ywa", "user001", "simple", "OK"))

    // post conditions

    val metadata = (testDir / "bags").children.next() / "metadata"

    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml")

    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml")

    // the rest of the content is tested in FileItemSpec
    (metadata / "files.xml").lines.map(_.trim).mkString("\n") should
      include("<dct:identifier>easy-file:35</dct:identifier>")
  }

  it should "report invalid file metadata" in {
    val invalidFileFoXml = XML.load(
      fileFoXml().serialize
        .split("\n")
        .filterNot(_.contains("<visibleTo>"))
        .mkString("\n")
        .inputStream
    )

    val app = new MockedApp()
    expectAUser(app.ldapContext)
    (app.fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
      Success(Seq("easy-file:35"))
    Map(
      "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
      "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
      "easy-file:35" -> invalidFileFoXml,
    ).foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.createBag("easy-dataset:13", bagDir, strict = true, europeana = false, app.filter) should matchPattern {
      case Failure(e) if e.getMessage == "easy-file:35 <visibleTo> not found" =>
    }
  }

  "addPayloads" should "export all files" in {
    val app = new MockedApp()
    val foXMLs = Map(
      "easy-file:1" -> fileFoXml(id = 1, name = "a.txt"),
      "easy-file:2" -> fileFoXml(id = 2, name = "b.txt"),
      "easy-file:3" -> fileFoXml(id = 3, name = "c.txt"),
    )
    foXMLs.foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      (app.fedoraProvider.disseminateDatastream(_: String, _: String)
        ) expects(id, "EASY_FILE") once() returning managed("blablabla".inputStream)
    }

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.addPayloads(emptyBag(bagDir), ALL_FILES, foXMLs.keys.toList) shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("original", "c.txt", "b.txt", "a.txt")
  }

  it should "export largest image as payload" in {
    val app = new MockedApp()
    val foXMLs = Map(
      "easy-file:1" -> fileFoXml(id = 1, name = "b.png", mimeType = "image/png", size = 10, accessibleTo = "ANONYMOUS"),
      "easy-file:2" -> fileFoXml(id = 2, name = "c.png", mimeType = "image/png", size = 20, accessibleTo = "ANONYMOUS"),
      "easy-file:3" -> fileFoXml(id = 3, name = "d.png", mimeType = "image/png", size = 15, accessibleTo = "ANONYMOUS"),
      "easy-file:4" -> fileFoXml(id = 4, name = "e.pdf", mimeType = "application/pdf", size = 15),
      "easy-file:5" -> fileFoXml(id = 5, name = "a.txt"),
    )
    foXMLs.foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }
    (app.fedoraProvider.disseminateDatastream(_: String, _: String)
      ) expects("easy-file:2", "EASY_FILE") once() returning managed("blablabla".inputStream)

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.addPayloads(emptyBag(bagDir), LARGEST_IMAGE, foXMLs.keys.toList) shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("original", "c.png")
  }

  it should "fall back to pdf files" in {
    val app = new MockedApp()
    val foXMLs = Map(
      "easy-file:1" -> fileFoXml(id = 1, name = "a.txt"),
      "easy-file:2" -> fileFoXml(id = 2, name = "b.pdf", mimeType = "application/pdf", size = 10, accessibleTo = "ANONYMOUS"),
      "easy-file:3" -> fileFoXml(id = 3, name = "c.pdf", mimeType = "application/pdf", size = 20, accessibleTo = "ANONYMOUS"),
      "easy-file:4" -> fileFoXml(id = 4, name = "d.pdf", mimeType = "application/pdf", size = 15, accessibleTo = "ANONYMOUS"),
      "easy-file:5" -> fileFoXml(id = 5, name = "e.png", mimeType = "image/png"),
    )
    foXMLs.foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }
    (app.fedoraProvider.disseminateDatastream(_: String, _: String)
      ) expects("easy-file:3", "EASY_FILE") once() returning managed("blablabla".inputStream)

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.addPayloads(emptyBag(bagDir), LARGEST_IMAGE, foXMLs.keys.toList) shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("original", "c.pdf")
  }

  it should "export only original files" in {
    val app = new MockedApp()
    val foXMLs = Map(
      "easy-file:1" -> fileFoXml(id = 1, name = "a.txt", location = "x"), // default: original
      "easy-file:2" -> fileFoXml(id = 2, name = "b.pdf", mimeType = "application/pdf", size = 10, accessibleTo = "ANONYMOUS"),
      "easy-file:3" -> fileFoXml(id = 3, name = "c.pdf", mimeType = "application/pdf", size = 20, accessibleTo = "ANONYMOUS"),
      "easy-file:4" -> fileFoXml(id = 4, name = "d.pdf", mimeType = "application/pdf", size = 15, accessibleTo = "ANONYMOUS"),
      "easy-file:5" -> fileFoXml(id = 5, name = "e.png", mimeType = "image/png", size = 15, location = "x"),
    )
    foXMLs.foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }
    Seq(1, 5).foreach(i =>
      (app.fedoraProvider.disseminateDatastream(_: String, _: String)
        ) expects(s"easy-file:$i", "EASY_FILE") once() returning managed("blablabla".inputStream)
    )
    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.addPayloads(emptyBag(bagDir), ALL_BUT_ORIGINAL, foXMLs.keys.toList) shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("x", "a.txt", "e.png")
  }

  it should "cause NoPayloadFilesException" in {
    val app = new MockedApp()
    val foXMLs = Map(
      "easy-file:1" -> fileFoXml(id = 1, name = "a.txt"),
      "easy-file:5" -> fileFoXml(id = 5, name = "e.png", mimeType = "image/png"),
    )
    foXMLs.foreach { case (id, xml) =>
      (app.fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.addPayloads(emptyBag(bagDir), LARGEST_IMAGE, foXMLs.keys.toList) shouldBe
      Failure(NoPayloadFilesException())
  }

  private def emptyBag(bagDir: File) = DansV0Bag
    .empty(bagDir)
    .getOrElse(fail("could not create test bag"))

  private def expectAUser(ldapContext: => InitialLdapContext) = {
    val result = mock[NamingEnumeration[SearchResult]]
    result.hasMoreElements _ expects() returning true
    val attributes = new BasicAttributes {
      put("displayname", "U.Ser")
      put("mail", "does.not.exist@dans.knaw.nl")
    }
    result.nextElement _ expects() returning new SearchResult("", null, attributes)
    (ldapContext.search(_: String, _: String, _: SearchControls)) expects(*, *, *) returning result
  }
}
