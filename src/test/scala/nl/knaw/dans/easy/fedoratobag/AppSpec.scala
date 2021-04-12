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

import better.files.{ File, _ }
import nl.knaw.dans.easy.fedoratobag.OutputFormat.{ AIP, SIP }
import nl.knaw.dans.easy.fedoratobag.TransformationType.ORIGINAL_VERSIONED
import nl.knaw.dans.easy.fedoratobag.filter.{ BagIndex, InvalidTransformationException, SimpleDatasetFilter }
import nl.knaw.dans.easy.fedoratobag.fixture._
import org.scalamock.scalatest.MockFactory
import resource.managed

import java.io.StringWriter
import java.util.UUID
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class AppSpec extends TestSupportFixture with FileFoXmlSupport with BagIndexSupport with MockFactory with FileSystemSupport with AudienceSupport {
  implicit val logFile: File = testDir / "log.txt"

  private class AppWithMockedServices(configuration: Configuration = new Configuration("test-version", null, null, null, testDir / "staging", AbrMappings(File("src/main/assembly/dist/cfg/EMD_acdm.xsl"))),
                                     ) extends EasyFedoraToBagApp(configuration) {
    private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
    override lazy val bagIndex: BagIndex = mockBagIndexRespondsWith(body = "<result/>", code = 200)
    val filter: SimpleDatasetFilter = new SimpleDatasetFilter(targetIndex = bagIndex)

    def expectAUser(): Unit = {
      val result = mock[NamingEnumeration[SearchResult]]
      result.hasMoreElements _ expects() returning true
      val attributes = new BasicAttributes {
        put("displayname", "U.Ser")
        put("mail", "does.not.exist@dans.knaw.nl")
      }
      result.nextElement _ expects() returning new SearchResult("", null, attributes)
      (ldapContext.search(_: String, _: String, _: SearchControls)) expects(*, *, *) once() returning result
    }

    // make almost private method available for tests
    override def createBag(datasetId: DatasetId, bagDir: File, options: Options, maybeFirstBagVersion: Option[BagVersion] = None): Try[DatasetInfo] =
      super.createBag(datasetId, bagDir, options)
  }

  "createExport" should "produce two bags" in {
    val app = new AppWithMockedServices() {
      Map(
        "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
        "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
        "easy-file:35" -> fileFoXml(digest = digests("acabadabra")),
        "easy-file:36" -> fileFoXml(id = 36, location = "x", accessibleTo = "ANONYMOUS", digest = digests("rabarbera")),
        "easy-file:37" -> fileFoXml(id = 37, accessibleTo = "NONE", name = "b.txt", digest = digests("barbapappa")),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      Seq(
        (1, "easy-dataset:17", "ADDITIONAL_LICENSE", "lalala"),
        (1, "easy-dataset:17", "DATASET_LICENSE", "blablabla"),
        (2, "easy-file:35", "EASY_FILE", "acabadabra"),
        (1, "easy-file:36", "EASY_FILE", "rabarbera"),
        (1, "easy-file:37", "EASY_FILE", "barbapappa"),
      ).foreach { case (n, objectId, streamId, content) =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(objectId, streamId
        ) returning managed(content.inputStream) repeat n
      }
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
        Success(Seq("easy-file:35", "easy-file:36", "easy-file:37"))
    }
    // end of mocking

    val sw = new StringWriter()
    app.createOriginalVersionedExport(
      Iterator("easy-dataset:17"),
      (testDir / "output").createDirectories,
      Options(SimpleDatasetFilter(allowOriginalAndOthers = true), ORIGINAL_VERSIONED),
      SIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post condition

    sw.toString should fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:17,.+,.+,10.17026/test-Iiib-z9p-4ywa,user001,original-versioned,OK
        |""".stripMargin

    // post condition: the data folders of both bags have the same number of files as their files.xml

    testDir.listRecursively.withFilter(_.name == "data").map(_.parent).foreach { bag =>
      val nrOfFiles = (bag / "data").listRecursively.filterNot(_.isDirectory).size
      (XML.loadFile((bag / "metadata" / "files.xml").toJava) \\ "file").theSeq.size shouldBe
        nrOfFiles
    }
  }

  it should "produce the second bag as first and only" in {
    val app = new AppWithMockedServices() {
      Map(
        "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
        "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
        "easy-file:36" -> fileFoXml(id = 36, location = "original/x", visibleTo = "NONE", accessibleTo = "NONE", name = "b.txt", digest = digests("barbapappa")),
        "easy-file:37" -> fileFoXml(id = 37, location = "x", accessibleTo = "ANONYMOUS", name = "b.txt", digest = digests("barbapappa")),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      Seq(
        (1, "easy-dataset:17", "ADDITIONAL_LICENSE", "lalala"),
        (1, "easy-dataset:17", "DATASET_LICENSE", "blablabla"),
        (1, "easy-file:37", "EASY_FILE", "barbapappa"),
      ).foreach { case (n, objectId, streamId, content) =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(objectId, streamId
        ) returning managed(content.inputStream) repeat n
      }
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
        Success(Seq("easy-file:36", "easy-file:37"))
    }
    // end of mocking

    val sw = new StringWriter()
    app.createOriginalVersionedExport(
      Iterator("easy-dataset:17"),
      (testDir / "output").createDirectories,
      Options(SimpleDatasetFilter(allowOriginalAndOthers = true), ORIGINAL_VERSIONED),
      SIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post condition

    sw.toString should fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:17,.+,,10.17026/test-Iiib-z9p-4ywa,user001,original-versioned without second bag,OK
        |""".stripMargin

    // post condition: just one bag

    (testDir / "output").list should have size 1
  }

  it should "produce a single bag" in {
    val app = new AppWithMockedServices() {
      Map(
        "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
        "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
        "easy-file:35" -> fileFoXml(digest = digests("acabadabra")),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      Seq(
        (1, "easy-dataset:17", "ADDITIONAL_LICENSE", "lalala"),
        (1, "easy-dataset:17", "DATASET_LICENSE", "blablabla"),
        (1, "easy-file:35", "EASY_FILE", "acabadabra"),
      ).foreach { case (n, objectId, streamId, content) =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(objectId, streamId
        ) returning managed(content.inputStream) repeat n
      }
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
        Success(Seq("easy-file:35"))
    }
    // end of mocking

    val sw = new StringWriter()
    app.createOriginalVersionedExport(
      Iterator("easy-dataset:17"),
      (testDir / "output").createDirectories,
      Options(new SimpleDatasetFilter(), ORIGINAL_VERSIONED),
      SIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")
    sw.toString should fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:17,.+,,10.17026/test-Iiib-z9p-4ywa,user001,original-versioned without second bag,OK
        |""".stripMargin
    val Array(_,line) = sw.toString.split("\n")
    testDir.listRecursively.filter(_.name == "dataset.xml").toSeq.head.contentAsString should
      include ("""<dc:title xml:lang="nld">as
                 |                        with another line</dc:title>""".stripMargin)
  }

  it should "report a checksum mismatch" in {
    val app = new AppWithMockedServices() {
      Map(
        "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
        "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
        "easy-file:35" -> fileFoXml(digest = digests("barbapappa")),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      Seq(
        ("easy-dataset:17", "ADDITIONAL_LICENSE", "lalala"),
        ("easy-dataset:17", "DATASET_LICENSE", "blablabla"),
        ("easy-file:35", "EASY_FILE", "acabadabra"),
      ).foreach { case (objectId, streamId, content) =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(objectId, streamId
        ) returning managed(content.inputStream) once()
      }
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
        Success(Seq("easy-file:35"))
    }
    // end of mocking

    val sw = new StringWriter()
    app.createOriginalVersionedExport(
      Iterator("easy-dataset:17"),
      (testDir / "output").createDirectories,
      Options(SimpleDatasetFilter()),
      SIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")
    sw.toString should fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:17,.+,,,,-,FAILED: java.lang.Exception: Different checksums in fedora Some(.*) and exported bag Some(.*) for .*/data/original/something.txt
        |""".stripMargin
  }

  it should "report a duplicate file" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, location = "original/a", name = "x.txt", digest = digests("acabadabra")),
        "easy-file:2" -> fileFoXml(id = 2, location = "a", name = "x.txt", digest = digests("acabadabra")),
        "easy-file:3" -> fileFoXml(id = 3, name = "y.txt", digest = digests("acabadabra")),
        "easy-file:4" -> fileFoXml(id = 4, location = "a", name = "z.txt", digest = digests("lalala")),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1", "easy-file:2", "easy-file:3", "easy-file:4"))
    }

    // end of mocking

    val sw = new StringWriter()
    app.createOriginalVersionedExport(
      Iterator("easy-dataset:13"),
      (testDir / "output").createDirectories(),
      Options(SimpleDatasetFilter(allowOriginalAndOthers = true), ORIGINAL_VERSIONED),
      AIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post condition
    sw.toString.split("\n").last should fullyMatch regex
      s"easy-dataset:13,.*,,,,-,FAILED: .*InvalidTransformationException: duplicates in first bag: ; duplicates in second bag: a/x.txt .isOriginalVersioned==true."
  }

  "createBag" should "report not strict simple violation" in {
    val app = new AppWithMockedServices() {
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
        Success(Seq("easy-jumpoff:1"))
      Map(
        "ADDITIONAL_LICENSE" -> "lalala",
        "DATASET_LICENSE" -> "blablabla",
      ).foreach { case (streamId, content) =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)) expects("easy-dataset:17", streamId
        ) once() returning managed(content.inputStream)
      }
      Map(
        "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
        "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
    }

    // end of mocking

    val uuid = UUID.randomUUID
    val bagDir = testDir / "bags" / uuid.toString
    app.createBag("easy-dataset:17", bagDir, Options(app.filter, strict = false)) shouldBe
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
    val app = new AppWithMockedServices() {
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:17" once() returning
        Success(Seq("dans-jumpoff:1"))
      Map(
        "easy-discipline:77" -> audienceFoXML("easy-discipline:77", "D13200"),
        "easy-dataset:17" -> XML.loadFile((sampleFoXML / "DepositApi.xml").toJava),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
    }

    // end of mocking

    val uuid = UUID.randomUUID
    val bagDir = testDir / "bags" / uuid.toString
    app.createBag("easy-dataset:17", bagDir, Options(app.filter)) should matchPattern {
      case Failure(_: InvalidTransformationException) =>
    }
    (testDir / "bags") shouldNot exist
  }

  it should "process streaming" in {
    val app = new AppWithMockedServices() {
      expectAUser()
      Map(
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-file:35" -> fileFoXml(digest = digests("barbapappa")),
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(
        "easy-file:35",
        "EASY_FILE"
      ) once() returning managed("barbapappa".inputStream)
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:35"))
    }
    // end of mocking

    val uuid = UUID.randomUUID
    val bagDir = testDir / "bags" / uuid.toString
    app.createBag("easy-dataset:13", bagDir, Options(app.filter)) shouldBe
      Success(DatasetInfo(None, "10.17026/mocked-Iiib-z9p-4ywa", "urn:nbn:nl:ui:13-blablabla", "user001", Seq.empty))

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
        .replace("<visibleTo>ANONYMOUS</visibleTo>", "")
        .inputStream
    )

    val app = new AppWithMockedServices() {
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:35"))
      Map(
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-file:35" -> invalidFileFoXml,
      ).foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
    }

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.createBag("easy-dataset:13", bagDir, Options(app.filter)) should matchPattern {
      case Failure(e) if e.getMessage == "easy-file:35 <visibleTo> not found" =>
    }
  }

  it should "export all files" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1", "easy-file:2", "easy-file:3"))
      val foXMLs = Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, name = "a.txt", digest = digests("lalala")),
        "easy-file:2" -> fileFoXml(id = 2, name = "b.txt", digest = digests("lalala")),
        "easy-file:3" -> fileFoXml(id = 3, name = "c.txt", digest = digests("lalala")),
      )
      foXMLs.foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      Seq("easy-file:1", "easy-file:2", "easy-file:3").foreach { id =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)
          ) expects(id, "EASY_FILE") once() returning managed("lalala".inputStream)
      }
    }

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    val triedRecord = app.createBag("easy-dataset:13", bagDir, Options(app.filter))
    triedRecord shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("original", "c.txt", "b.txt", "a.txt")
  }

  it should "report an invalid checksum" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1"))
      val foXMLs = Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, name = "a.txt", digest = digests("acabadabra")),
      )
      foXMLs.foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      (fedoraProvider.disseminateDatastream(_: String, _: String)
        ) expects("easy-file:1", "EASY_FILE") once() returning managed("lalala".inputStream)
    }

    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    val triedRecord = app.createBag("easy-dataset:13", bagDir, Options(app.filter))
    triedRecord should matchPattern {
      case Failure(e: Exception) if e.getMessage.matches(
        "Different checksums in fedora Some(.*) and exported bag Some(.*) for .*/data/original/a.txt"
      ) =>
    }
  }

  it should "export largest image as payload" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1", "easy-file:2", "easy-file:3", "easy-file:4", "easy-file:5"))
      val foXMLs = Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, name = "b.png", mimeType = "image/png", size = 10, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:2" -> fileFoXml(id = 2, name = "c.png", mimeType = "image/png", size = 20, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:3" -> fileFoXml(id = 3, name = "d.png", mimeType = "image/png", size = 15, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:4" -> fileFoXml(id = 4, name = "e.pdf", mimeType = "application/pdf", size = 15, digest = digests("lalala")),
        "easy-file:5" -> fileFoXml(id = 5, name = "a.txt", digest = digests("lalala")),
      )
      foXMLs.foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      (fedoraProvider.disseminateDatastream(_: String, _: String)
        ) expects("easy-file:2", "EASY_FILE") once() returning managed("lalala".inputStream)
    }
    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.createBag("easy-dataset:13", bagDir, Options(app.filter, europeana = true)) shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("original", "c.png")
  }

  it should "fall back to largest pdf file" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1", "easy-file:2", "easy-file:3", "easy-file:4", "easy-file:5"))
      val foXMLs = Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, name = "a.txt"),
        "easy-file:2" -> fileFoXml(id = 2, name = "b.pdf", mimeType = "application/pdf", size = 10, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:3" -> fileFoXml(id = 3, name = "c.pdf", mimeType = "application/pdf", size = 20, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:4" -> fileFoXml(id = 4, name = "d.pdf", mimeType = "application/pdf", size = 15, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:5" -> fileFoXml(id = 5, name = "e.png", mimeType = "image/png"),
      )
      foXMLs.foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      (fedoraProvider.disseminateDatastream(_: String, _: String)
        ) expects("easy-file:3", "EASY_FILE") once() returning managed("lalala".inputStream)
    }
    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.createBag("easy-dataset:13", bagDir, Options(app.filter, europeana = true)) shouldBe a[Success[_]]
    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("original", "c.pdf")
  }

  it should "export only original files" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1", "easy-file:2", "easy-file:3", "easy-file:4", "easy-file:5"))
      val foXMLs = Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, name = "a.txt", location = "x", digest = digests("lalala")), // default: original
        "easy-file:2" -> fileFoXml(id = 2, name = "b.pdf", mimeType = "application/pdf", size = 10, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:3" -> fileFoXml(id = 3, name = "c.pdf", mimeType = "application/pdf", size = 20, accessibleTo = "ANONYMOUS", digest = digests("lalala")),
        "easy-file:4" -> fileFoXml(id = 4, name = "d.pdf", mimeType = "application/pdf", size = 15, accessibleTo = "NONE", digest = digests("lalala")),
        "easy-file:5" -> fileFoXml(id = 5, name = "e.png", mimeType = "image/png", size = 15, location = "x", digest = digests("lalala")),
      )
      foXMLs.foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
      Seq(2, 3, 4).foreach(i =>
        (fedoraProvider.disseminateDatastream(_: String, _: String)
          ) expects(s"easy-file:$i", "EASY_FILE") once() returning managed("lalala".inputStream)
      )
    }
    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.createBag("easy-dataset:13", bagDir, Options(SimpleDatasetFilter(allowOriginalAndOthers = true), ORIGINAL_VERSIONED))
      .map(_.nextBagFileInfos.map(_.path.toString).sortBy(identity)) shouldBe
      Success(Vector("original/b.pdf", "original/c.pdf", "x/a.txt", "x/e.png"))

    (bagDir / "data").listRecursively.toList.map(_.name) should
      contain theSameElementsAs List("c.pdf", "b.pdf", "d.pdf")
  }

  it should "cause NoPayloadFilesException" in {
    val app: AppWithMockedServices = new AppWithMockedServices() {
      expectAUser()
      (fedoraProvider.getSubordinates(_: String)) expects "easy-dataset:13" once() returning
        Success(Seq("easy-file:1", "easy-file:5"))
      val foXMLs = Map(
        "easy-dataset:13" -> XML.loadFile((sampleFoXML / "streaming.xml").toJava),
        "easy-discipline:6" -> audienceFoXML("easy-discipline:6", "D35400"),
        "easy-file:1" -> fileFoXml(id = 1, name = "a.txt"),
        "easy-file:5" -> fileFoXml(id = 5, name = "e.png", mimeType = "image/png"),
      )
      foXMLs.foreach { case (id, xml) =>
        (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
      }
    }
    // end of mocking

    val bagDir = testDir / "bags" / UUID.randomUUID.toString
    app.createBag("easy-dataset:13", bagDir, Options(app.filter, europeana = true)) shouldBe
      Failure(NoPayloadFilesException())
  }
}
