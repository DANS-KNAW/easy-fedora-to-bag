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

import java.io.{ FileInputStream, StringWriter }
import java.util.UUID

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.easy.fedoratobag.OutputFormat.{ AIP, SIP }
import nl.knaw.dans.easy.fedoratobag.filter.{ BagIndex, DatasetFilter, InvalidTransformationException, SimpleDatasetFilter }
import nl.knaw.dans.easy.fedoratobag.fixture.{ AudienceSupport, BagIndexSupport, FileSystemSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory
import resource.managed

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class AppSpec extends TestSupportFixture with BagIndexSupport with MockFactory with FileSystemSupport with AudienceSupport {
  implicit val logFile: File = testDir / "log.txt"

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

  private class MockedApp(configuration: Configuration = new Configuration("test-version", null, null, null, null, AbrMappings(File("src/main/assembly/dist/cfg/EMD_acdm.xsl"))),
                          mockedBagIndex: BagIndex = mockBagIndexRespondsWith(body = "<result/>", code = 200),
                         ) extends EasyFedoraToBagApp(configuration) {
    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
    override lazy val bagIndex: BagIndex = mockedBagIndex
    val filter: SimpleDatasetFilter = SimpleDatasetFilter(bagIndex)

    // make almost private method available for tests
    override def createBag(datasetId: DatasetId, bagDir: File, strict: Boolean, europeana: Boolean, datasetFilter: DatasetFilter): Try[CsvRecord] =
      super.createBag(datasetId, bagDir, strict, europeana, datasetFilter)
  }

  private class OverriddenApp(configuration: Configuration = null) extends MockedApp(configuration) {
    /** overrides the method called by the method under test */
    override def createBag(datasetId: DatasetId, outputDir: File, strict: Boolean, europeana: Boolean, datasetFilter: DatasetFilter): Try[CsvRecord] = {
      outputDir.parent.createDirectories()
      datasetId match {
        case _ if datasetId.startsWith("fatal") =>
          Failure(new FedoraClientException(300, "mocked exception"))
        case _ if datasetId.startsWith("notSimple") =>
          outputDir.createFile().writeText(datasetId)
          Failure(InvalidTransformationException("mocked"))
        case _ if !datasetId.startsWith("success") =>
          outputDir.createFile().writeText(datasetId)
          Failure(new Exception(datasetId))
        case _ =>
          outputDir.createFile().writeText(datasetId)
          Success(CsvRecord(datasetId, UUID.randomUUID(), doi = "testDOI", depositor = "testUser", transformationType = "simple", comment = "OK"))
      }
    }
  }

  "createSips" should "report success" in {
    val ids = Iterator("success:1", "notSimple:1", "whoops:1", "success:1")
    val outputDir = (testDir / "output").createDirectories()
    val stagingDir = testDir / "staging"
    val app = new OverriddenApp(Configuration(null, null, null, null, stagingDir, null))
    val printer = CsvRecord.csvFormat.print(new StringWriter()) // content verified with simpleTransforms
    val triedMessage = app.createExport(ids, outputDir, strict = true, europeana = false, SimpleDatasetFilter(), SIP)(printer)
    triedMessage shouldBe Success("no fedora/IO errors")

    // two directories with one entry each
    stagingDir.list.toList should have length 2
    stagingDir.listRecursively.toList should have length 4

    // two directories with two entries each
    outputDir.list.toList should have length 2
    outputDir.listRecursively.toList should have length 4
  }

  "createAips" should "report success" in {
    val ids = Iterator("success:1", "success:2")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val stagingDir = testDir / "staging"
    val app = new OverriddenApp(Configuration(null, null, null, null, stagingDir, null))
    app.createExport(ids, outputDir, strict = true, europeana = false, app.filter,AIP)(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")
    sw.toString should (fullyMatch regex
      """easyDatasetId,uuid,doi,depositor,transformationType,comment
        |success:1,.*,testDOI,testUser,simple,OK
        |success:2,.*,testDOI,testUser,simple,OK
        |""".stripMargin
      )
    outputDir.listRecursively.toSeq should have length 2
  }

  it should "report failure" in {
    val ids = Iterator("success:1", "failure:2", "notSimple:3", "success:4", "fatal:5", "success:6")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val stagingDir = testDir / "staging"
    val app = new OverriddenApp(Configuration(null, null, null, null, stagingDir, null))
    app.createExport(ids, outputDir, strict = true, europeana = false, app.filter,AIP)(CsvRecord.csvFormat.print(sw)) should matchPattern {
      case Failure(t) if t.getMessage == "mocked exception" =>
    }
    sw.toString should (fullyMatch regex
      """easyDatasetId,uuid,doi,depositor,transformationType,comment
        |success:1,.*,testDOI,testUser,simple,OK
        |failure:2,.*,,,simple,FAILED: java.lang.Exception: failure:2
        |notSimple:3,.*,,,simple,FAILED: .*InvalidTransformationException: mocked
        |success:4,.*,testDOI,testUser,simple,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 2
    stagingDir.list.toSeq should have length 2
  }

  "createBag" should "process DepositApi" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map("easy-discipline:77" -> "D13200"))
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "DepositApi.xml",  sampleFoXML / "easy-file-35.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "additional-license").write("lalala"),
      (testDir / "dataset-license").write("blablabla"),
      mockContentOfFile35,
    )
    expectedSubordinates(app.fedoraProvider, "easy-file:35")

    val uuid = UUID.randomUUID
    app.createBag("easy-dataset:17", testDir / "bags" / uuid.toString, strict = true, europeana = false, app.filter) shouldBe
      Success(CsvRecord("easy-dataset:17", uuid, "10.17026/test-Iiib-z9p-4ywa", "user001", "simple", "OK"))

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
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map("easy-discipline:77" -> "D13200"))
    expectedSubordinates(app.fedoraProvider, "easy-jumpoff:1")
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "DepositApi.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "additional-license").write("lalala"),
      (testDir / "dataset-license").write("blablabla"),
    )

    val uuid = UUID.randomUUID
    app.createBag("easy-dataset:17", testDir / "bags" / uuid.toString, strict = false, europeana = false, app.filter) shouldBe
      Failure(NoPayloadFilesException())

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
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map("easy-discipline:77" -> "D13200"))
    expectedSubordinates(app.fedoraProvider, "easy-jumpoff:1")
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "DepositApi.xml")

    val uuid = UUID.randomUUID
    app.createBag("easy-dataset:17", testDir / "bags" / uuid.toString, strict = true, europeana = false, app.filter) should matchPattern {
      case Failure(_: InvalidTransformationException) =>
    }

    (testDir / "bags") shouldNot exist
  }

  it should "process streaming" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map(
      "easy-discipline:6" -> "D35400",
    ))
    expectAUser(app.ldapContext)
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "streaming.xml", sampleFoXML / "easy-file-35.xml")
    expectedSubordinates(app.fedoraProvider, "easy-file:35")
    expectedManagedStreams(app.fedoraProvider, mockContentOfFile35)

    val uuid = UUID.randomUUID
    app.createBag("easy-dataset:13", testDir / "bags" / uuid.toString, strict = true, europeana = false, app.filter) shouldBe
      Success(CsvRecord("easy-dataset:13", uuid, "10.17026/mocked-Iiib-z9p-4ywa", "user001", "simple", "OK"))

    val metadata = (testDir / "bags").children.next() / "metadata"

    metadata.list.toSeq.map(_.name)
      .sortBy(identity) shouldBe Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml")

    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml")

    // the rest of the content is tested in FileItemSpec
    (metadata / "files.xml").lines.map(_.trim).mkString("\n") should
      include("<dct:identifier>easy-file:35</dct:identifier>")
  }

  it should "report invalid file metadata" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map(
      "easy-discipline:6" -> "D35400",
    ))

    expectAUser(app.ldapContext)
    expectedFoXmls(
      app.fedoraProvider,
      sampleFoXML / "streaming.xml",
      (testDir / "easy-file-35.xml").writeText(
        (sampleFoXML / "easy-file-35.xml").contentAsString.split("\n")
          .filterNot(_.contains("<visibleTo>")).mkString("\n")
      ),
    )
    expectedSubordinates(app.fedoraProvider, "easy-file:35")

    app.createBag("easy-dataset:13", testDir / "bags" / UUID.randomUUID.toString, strict = true, europeana = false, app.filter) should matchPattern {
      case Failure(e) if e.getMessage == "easy-file:35 <visibleTo> not found" =>
    }
  }

  private def mockContentOfFile35 = {
    (testDir / "something.txt").writeText("mocked content of easy-file:35")
  }

  private def expectedSubordinates(fedoraProvider: => FedoraProvider, expectedIds: String*): Unit = {
    (fedoraProvider.getSubordinates(_: String)) expects * once() returning Success(expectedIds)
  }

  private def expectedManagedStreams(fedoraProvider: => FedoraProvider, expectedObjects: File*): Unit = {
    expectedObjects.foreach(file =>
      (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(*, *) once() returning
        managed(new FileInputStream(file.toJava))
    )
  }

  private def expectedFoXmls(fedoraProvider: => FedoraProvider, expectedObjects: File*): Unit = {
    expectedObjects.foreach(file =>
      (fedoraProvider.loadFoXml(_: String)) expects * once() returning
        Try(XML.loadFile(file.toJava))
    )
  }

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
