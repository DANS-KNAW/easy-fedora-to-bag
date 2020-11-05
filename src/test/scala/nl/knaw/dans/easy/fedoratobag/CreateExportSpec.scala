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

import java.io.StringWriter
import java.util.UUID

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.easy.fedoratobag.OutputFormat.{ AIP, SIP }
import nl.knaw.dans.easy.fedoratobag.filter.{ BagIndex, InvalidTransformationException, SimpleDatasetFilter }
import nl.knaw.dans.easy.fedoratobag.fixture._
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success, Try }

class CreateExportSpec extends TestSupportFixture with FileFoXmlSupport with BagIndexSupport with MockFactory with FileSystemSupport with AudienceSupport {
  implicit val logFile: File = testDir / "log.txt"
  private val stagingDir = testDir / "staging"

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

  private class OverriddenApp() extends EasyFedoraToBagApp(Configuration(null, null, null, null, stagingDir, null)) {
    override lazy val fedoraProvider: FedoraProvider = null
    override lazy val ldapContext: InitialLdapContext = null
    override lazy val bagIndex: BagIndex = null
    val filter: SimpleDatasetFilter = SimpleDatasetFilter(bagIndex)

    /** mocks the method called by the method under test */
    override def createFirstBag(datasetId: DatasetId, outputDir: File, options: Options): Try[(Seq[FileInfo],CsvRecord)] = {
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
          Success((Seq.empty,CsvRecord(datasetId, UUID.randomUUID(), doi = "testDOI", depositor = "testUser", transformationType = "simple", comment = "OK")))
      }
    }
  }

  "createSips" should "report success" in {
    val ids = Iterator("success:1", "notSimple:1", "whoops:1", "success:1")
    val outputDir = (testDir / "output").createDirectories()
    val app = new OverriddenApp()
    val sw = new StringWriter()

    // end of mocking

    app.createExport(ids, outputDir, Options(SimpleDatasetFilter()), SIP)(CsvRecord.csvFormat.print(sw)) shouldBe
      Success("no fedora/IO errors")

    // two directories with one entry each
    stagingDir.list.toList should have length 2
    stagingDir.listRecursively.toList should have length 4

    // two directories with two entries each
    outputDir.list.toList should have length 2
    outputDir.listRecursively.toList should have length 4

    val csvContent = sw.toString // rest of the content tested with createAips
    outputDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
  }

  "createAips" should "report success" in {
    val ids = Iterator("success:1", "success:2")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val app = new OverriddenApp()

    // end of mocking

    app.createExport(ids, outputDir, Options(app.filter), AIP)(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      """easyDatasetId,uuid,doi,depositor,transformationType,comment
        |success:1,.*,testDOI,testUser,simple,OK
        |success:2,.*,testDOI,testUser,simple,OK
        |""".stripMargin
      )
    outputDir.listRecursively.toSeq should have length 2
    outputDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
  }

  it should "report failure" in {
    val ids = Iterator("success:1", "failure:2", "notSimple:3", "success:4", "fatal:5", "success:6")
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val app = new OverriddenApp()

    // end of mocking

    app.createExport(ids, outputDir, Options(app.filter), AIP)(CsvRecord.csvFormat.print(sw)) should matchPattern {
      case Failure(t) if t.getMessage == "mocked exception" =>
    }

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      """easyDatasetId,uuid,doi,depositor,transformationType,comment
        |success:1,.*,testDOI,testUser,simple,OK
        |failure:2,.*,,,simple,FAILED: java.lang.Exception: failure:2
        |notSimple:3,.*,,,simple,FAILED: .*InvalidTransformationException: mocked
        |success:4,.*,testDOI,testUser,simple,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 2
    stagingDir.list.toSeq should have length 2
    outputDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
    stagingDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
  }
}
