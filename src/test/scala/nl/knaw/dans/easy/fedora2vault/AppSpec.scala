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

import java.io.{ FileInputStream, StringWriter }
import java.util.UUID

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.easy.fedora2vault.check.{ InvalidTransformationException, SimpleChecker, TransformationChecker }
import nl.knaw.dans.easy.fedora2vault.fixture.{ AudienceSupport, BagIndexSupport, FileSystemSupport, TestSupportFixture }
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

  private class MockedApp(mockedBagIndex: BagIndex = mockBagIndexRespondsWith(body = "<result/>", code = 200)
                         ) extends EasyFedora2vaultApp(null) {
    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
    override lazy val bagIndex: BagIndex = mockedBagIndex
    val simpleChecker: SimpleChecker = SimpleChecker(bagIndex)
  }

  private class OverriddenApp extends MockedApp {
    /** overrides the method called by the method under test */
    override def simpleTransform(datasetId: DatasetId, outputDir: File, strict: Boolean)
                                (implicit transformationChecker: TransformationChecker): Try[CsvRecord] = {
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
          Success(CsvRecord(datasetId, UUID.randomUUID(), "", "", "simple", "OK"))
      }
    }
  }

  "simpleTransforms" should "report success" in {
    val ids = Iterator("success:1", "success:2")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val app = new OverriddenApp()
    app.simpleTransForms(ids, outputDir, strict = true, sw)(app.simpleChecker) shouldBe Success("no fedora/IO errors")
    sw.toString should (fullyMatch regex
      """easyDatasetId,uuid,doi,depositor,transformationType,comment
        |success:1,.*,,,simple,OK
        |success:2,.*,,,simple,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 2
  }

  it should "report failure" in {
    val ids = Iterator("success:1", "failure:2", "notSimple:3", "success:4", "fatal:5", "success:6")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val app = new OverriddenApp()
    app.simpleTransForms(ids, outputDir, strict = true, sw)(app.simpleChecker) should matchPattern {
      case Failure(t) if t.getMessage == "mocked exception" =>
    }
    sw.toString should (fullyMatch regex
      """easyDatasetId,uuid,doi,depositor,transformationType,comment
        |success:1,.*,,,simple,OK
        |failure:2,.*,,,simple,FAILED: java.lang.Exception: failure:2
        |notSimple:3,.*,,,simple,FAILED: nl.knaw.dans.easy.fedora2vault.check.InvalidTransformationException: mocked
        |success:4,.*,,,simple,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 4
  }

  "simpleTransform" should "process DepositApi" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map("easy-discipline:77" -> "D13200"))
    expectedSubordinates(app.fedoraProvider)
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "DepositApi.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "additional-license").write("lalala"),
      (testDir / "dataset-license").write("blablabla"),
    )

    val uuid = UUID.randomUUID
    app.simpleTransform("easy-dataset:17", testDir / "bags" / uuid.toString, strict = true)(app.simpleChecker) shouldBe
      Success(CsvRecord("easy-dataset:17", uuid, "10.17026/test-Iiib-z9p-4ywa", "user001", "simple", "OK"))

    val metadata = (testDir / "bags").children.next() / "metadata"
    (metadata / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (metadata / "license.pdf").contentAsString shouldBe "lalala"
    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf")
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
    app.simpleTransform("easy-dataset:17", testDir / "bags" / uuid.toString, strict = false)(app.simpleChecker) shouldBe
      Success(CsvRecord("easy-dataset:17", uuid, "10.17026/test-Iiib-z9p-4ywa", "user001", "not strict simple", "Violates 2: has jump off"))

    val metadata = (testDir / "bags").children.next() / "metadata"
    (metadata / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (metadata / "license.pdf").contentAsString shouldBe "lalala"
    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf")
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
    app.simpleTransform("easy-dataset:17", testDir / "bags" / uuid.toString, strict = true)(app.simpleChecker) should matchPattern {
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
    app.simpleTransform("easy-dataset:13", testDir / "bags" / uuid.toString, strict = true)(app.simpleChecker) shouldBe
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
    expectedManagedStreams(app.fedoraProvider, mockContentOfFile35)

    app.simpleTransform("easy-dataset:13", testDir / "bags" / UUID.randomUUID.toString, strict = true)(app.simpleChecker) should matchPattern {
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
