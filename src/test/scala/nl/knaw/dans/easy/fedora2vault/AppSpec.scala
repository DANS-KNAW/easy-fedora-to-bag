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

import java.io.FileInputStream
import java.lang.{ StringBuilder => JavaStringBuilder }
import java.util.UUID

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.easy.fedora2vault.Command.FeedBackMessage
import nl.knaw.dans.easy.fedora2vault.TransformationType.SIMPLE
import nl.knaw.dans.easy.fedora2vault.fixture.{ AudienceSupport, FileSystemSupport, TestSupportFixture }
import org.apache.commons.csv.CSVPrinter
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
    override def simpleTransform(outputDir: File)(datasetId: DatasetId)(implicit printer: CSVPrinter): Try[FeedBackMessage] = {
      if (datasetId.startsWith("fatal"))
        Failure(new FedoraClientException(300, "mocked exception"))
      else if (!datasetId.startsWith("success")) {
        outputDir.createFile().writeText(datasetId)
        Failure(new Exception(datasetId))
      } else {
        outputDir.createFile().writeText(datasetId)
        CsvRecord(datasetId, "", "", SIMPLE, UUID.randomUUID(), "OK").print
      }
    }
  }

  "simpleTransforms" should "report success" in {
    val input = (testDir / "input").write(
      """success:1
        |success:2
        |""".stripMargin
    )
    val outputDir = (testDir / "output").createDirectories()
    val sb = new JavaStringBuilder()
    new OverriddenApp().simpleTransForms(input, outputDir)(csvPrinter(sb)) shouldBe Success(
      s"""All datasets in $input
         | saved as bags in $outputDir
         | """.stripMargin
    )
    outputDir.list.toSeq should have length 2
    sb.toString should (fullyMatch regex
      """easyDatasetId,doi,depositor,transformationType,uuid,comment
        |success:1,,,simple,.*,OK
        |success:2,,,simple,.*,OK
        |""".stripMargin
      )
  }

  it should "report failure" in {
    val input = (testDir / "input").write(
      """success:1
        |failure:2
        |success:3
        |fatal:4
        |success:5
        |""".stripMargin
    )
    val outputDir = (testDir / "output").createDirectories()
    val sb = new JavaStringBuilder()
    new OverriddenApp().simpleTransForms(input, outputDir)(csvPrinter(sb)) should matchPattern {
      case Failure(t) if t.getMessage == "mocked exception" =>
    }
    outputDir.list.toSeq should have length 3
    sb.toString should (fullyMatch regex
      """easyDatasetId,doi,depositor,transformationType,uuid,comment
        |success:1,,,simple,.*,OK
        |failure:2,,,simple,.*,FAILED: java.lang.Exception: failure:2
        |success:3,,,simple,.*,OK
        |""".stripMargin
      )
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
      (testDir / "manifest-sha1.txt").write("rabarbera"),
    )

    val sb = new JavaStringBuilder()
    app.simpleTransform(testDir / "bags" / UUID.randomUUID.toString)("easy-dataset:17")(csvPrinter(sb)) shouldBe Success("OK")
    sb.toString.startsWith("easy-dataset:17\t10.17026/test-Iiib-z9p-4ywa\tuser001\tsimple\t")
    val bag = (testDir / "bags").children.next()
    (bag / "metadata" / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (bag / "metadata" / "license.pdf").contentAsString shouldBe "lalala"
    (bag / "metadata").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf")
    (bag / "metadata" / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf", "message-from-depositor.txt")
  }

  it should "process streaming" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map(
      "easy-discipline:6" -> "D35400",
    ))
    expectAUser(app.ldapContext)
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "streaming.xml")
    expectedSubordinates(app.fedoraProvider)

    val sb = new JavaStringBuilder()
    app.simpleTransform(testDir / "bags" / UUID.randomUUID.toString)("easy-dataset:13")(csvPrinter(sb)) shouldBe Success("OK")
    sb.toString.startsWith("easy-dataset:13\tnull\tuser001\tsimple\t")
    val bag = (testDir / "bags").children.next()
    (bag / "metadata").list.toSeq.map(_.name)
      .sortBy(identity) shouldBe Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml")
    (bag / "metadata" / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml")
  }

  private def csvPrinter(sb: JavaStringBuilder): CSVPrinter = {
    CsvRecord.csvFormat.print(sb)
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
