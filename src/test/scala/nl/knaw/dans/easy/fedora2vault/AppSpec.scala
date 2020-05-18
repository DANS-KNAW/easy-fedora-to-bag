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
import nl.knaw.dans.easy.fedora2vault.TransformationType.SIMPLE
import nl.knaw.dans.easy.fedora2vault.fixture.{ AudienceSupport, FileSystemSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory
import resource.managed

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class AppSpec extends TestSupportFixture with MockFactory with FileSystemSupport with AudienceSupport {
  implicit val logFile: File = testDir / "log.txt"

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

  private class MockedApp() extends EasyFedora2vaultApp(null) {
    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
  }

  private class OverriddenApp extends MockedApp {
    /** overrides the method called by the method under test */
    override def simpleTransform(datasetId: DatasetId, outputDir: File): Try[CsvRecord] = {
      if (datasetId.startsWith("fatal"))
        Failure(new FedoraClientException(300, "mocked exception"))
      else if (!datasetId.startsWith("success")) {
        outputDir.createFile().writeText(datasetId)
        Failure(new Exception(datasetId))
      } else {
        outputDir.createFile().writeText(datasetId)
        Success(CsvRecord(datasetId, "", "", SIMPLE, UUID.randomUUID(), "OK"))
      }
    }
  }

  "simpleTransforms" should "report success" in {
    val ids = Iterator("success:1", "success:2")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    new OverriddenApp().simpleTransForms(ids, outputDir, sw) shouldBe Success("OK")
    sw.toString should (fullyMatch regex
      """easyDatasetId,doi,depositor,transformationType,uuid,comment
        |success:1,,,simple,.*,OK
        |success:2,,,simple,.*,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 2
  }

  it should "report failure" in {
    val ids = Iterator("success:1", "failure:2", "success:3", "fatal:4", "success:5")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    new OverriddenApp().simpleTransForms(ids, outputDir, sw) should matchPattern {
      case Failure(t) if t.getMessage == "mocked exception" =>
    }
    sw.toString should (fullyMatch regex
      """easyDatasetId,doi,depositor,transformationType,uuid,comment
        |success:1,,,simple,.*,OK
        |failure:2,,,simple,.*,FAILED: java.lang.Exception: failure:2
        |success:3,,,simple,.*,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 3
  }

  "simpleTransform" should "process DepositApi" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map(
      "easy-discipline:77" -> "D13200",
    ))
    expectedSubordinates(app.fedoraProvider)
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "DepositApi.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "additional-license").write("lalala"),
      (testDir / "dataset-license").write("blablabla"),
    )

    app.simpleTransform("easy-dataset:17", testDir / "bags" / UUID.randomUUID.toString) should matchPattern {
      case Success(CsvRecord("easy-dataset:17", "10.17026/test-Iiib-z9p-4ywa", "user001", SIMPLE, _, "OK")) =>
    }
    val metadata = (testDir / "bags").children.next() / "metadata"
    (metadata / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (metadata / "license.pdf").contentAsString shouldBe "lalala"
    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf")
    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf", "message-from-depositor.txt")
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
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "something.txt").writeText("mocked content of easy-file:35")
    )

    app.simpleTransform("easy-dataset:13", testDir / "bags" / UUID.randomUUID.toString) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        // failure because the mocked data stream doesn't match the original fedora stream
        "checksum error fedora[b5fbe5be379583d600f86610cf437c424567237e] bag[dd466d19481a28ba8577e7b3f029e496027a3309] easy-file:35 data/original/P1130783.JPG" =>
    }
    val metadata = (testDir / "bags").children.next() / "metadata"
    metadata.list.toSeq.map(_.name)
      .sortBy(identity) shouldBe Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml")
    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml")
    (metadata / "files.xml").contentAsString.split("\n").map(_.trim).mkString("\n") shouldBe
      """<?xml version='1.0' encoding='UTF-8'?>
        |<files
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/" xmlns:dcterms="http://purl.org/dc/terms/">
        |<file filepath="data/original/P1130783.JPG">
        |<dcterms:title>P1130783.JPG</dcterms:title>
        |<dcterms:format>image/jpeg</dcterms:format>
        |<accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
        |<visibleToRights>ANONYMOUS</visibleToRights>
        |</file>
        |</files>""".stripMargin
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
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "something.txt").writeText("don't care")
    )

    app.simpleTransform("easy-dataset:13", testDir / "bags" / UUID.randomUUID.toString) should matchPattern {
      case Failure(e) if e.getMessage == "No <visibleTo> in EASY_FILE_METADATA for easy-file:35" =>
    }
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
