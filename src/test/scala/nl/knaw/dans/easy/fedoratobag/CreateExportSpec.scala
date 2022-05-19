/*
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

import com.yourmediashelf.fedora.client.FedoraClientException
import nl.knaw.dans.easy.fedoratobag.OutputFormat.{ AIP, SIP }
import nl.knaw.dans.easy.fedoratobag.filter.{ InvalidTransformationException, SimpleDatasetFilter }
import nl.knaw.dans.easy.fedoratobag.fixture._
import org.scalamock.scalatest.MockFactory

import java.io.StringWriter
import scala.util.{ Failure, Success }

class CreateExportSpec extends TestSupportFixture with DelegatingApp with FileFoXmlSupport with BagIndexSupport with MockFactory with FileSystemSupport with AudienceSupport {
  private val stagingDir = testDir / "staging"

  "createSips" should "report success" in {
    val outputDir = (testDir / "output").createDirectories()
    stagingDir.createDirectories()
    val sw = new StringWriter()

    val createBagExpects = Seq(
      "easy-dataset:1" -> Success(DatasetInfo(None, "mocked-doi1", "", "user001", withPayload = true)),
      "easy-dataset:2" -> Failure(InvalidTransformationException("mocked")),
      "easy-dataset:3" -> Failure(new Exception("easy-dataset:3")),
      "easy-dataset:4" -> Success(DatasetInfo(None, "mocked-doi4", "", "user001", withPayload = true)),
    )

    // end of mocking

    delegatingApp(stagingDir, createBagExpects).createExport2(
      List(InputFileRecord("easy-dataset:1"), InputFileRecord("easy-dataset:2"), InputFileRecord("easy-dataset:3"), InputFileRecord("easy-dataset:4")),
      Seq.empty, outputDir, Options(SimpleDatasetFilter()), SIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe
      Success("no fedora/IO errors")

    // post conditions

    stagingDir.list.toList should have length 2
    outputDir.list.toList should have length 2

    val csvContent = sw.toString
    outputDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
    csvContent should (fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:1,.+,,mocked-doi1,user001,simple,OK
        |easy-dataset:2,.+,,,,-,FAILED: .*InvalidTransformationException: mocked
        |easy-dataset:3,.+,,,,-,FAILED: java.lang.Exception: easy-dataset:3
        |easy-dataset:4,.+,,mocked-doi4,user001,simple,OK
        |""".stripMargin
      )
  }

  "createAips" should "report success" in {
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val createBagExpects = Seq(
      "easy-dataset:1" -> Success(DatasetInfo(None, "mocked-doi1", "", "testUser", withPayload = true)),
      "easy-dataset:2" -> Success(DatasetInfo(None, "mocked-doi2", "", "testUser", withPayload = true)),
    )

    // end of mocking

    val app = delegatingApp(stagingDir, createBagExpects)
    app.createExport2(
      List(InputFileRecord("easy-dataset:1"), InputFileRecord("easy-dataset:2")),
      Seq.empty, outputDir, Options(SimpleDatasetFilter(targetIndex = app.bagIndex)), AIP
    )(CsvRecord.csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:1,.+,,mocked-doi1,testUser,simple,OK
        |easy-dataset:2,.+,,mocked-doi2,testUser,simple,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 2
    outputDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
  }

  it should "report failure" in {
    val outputDir = (testDir / "output").createDirectories()
    val sw = new StringWriter()
    val createBagExpects = Seq(
      "easy-dataset:1" -> Success(DatasetInfo(None, "mocked-doi1", "", "testUser", withPayload = true)),
      "easy-dataset:2" -> Failure(new Exception("easy-dataset:2")),
      "easy-dataset:3" -> Failure(InvalidTransformationException("mocked")),
      "easy-dataset:4" -> Success(DatasetInfo(None, "mocked-doi4", "", "testUser", withPayload = true)),
      "easy-dataset:5" -> Failure(new FedoraClientException(300, "mocked exception")),
    )

    // end of mocking

    val app = delegatingApp(stagingDir, createBagExpects)
    app.createExport2(
      List(InputFileRecord("easy-dataset:1"), InputFileRecord("easy-dataset:2"), InputFileRecord("easy-dataset:3"), InputFileRecord("easy-dataset:4"), InputFileRecord("easy-dataset:5")),
      Seq.empty, outputDir, Options(SimpleDatasetFilter(targetIndex = app.bagIndex)), AIP
    )(CsvRecord.csvFormat.print(sw)) should matchPattern {
      case Failure(t) if t.getMessage == "mocked exception" =>
    }

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:1,.+,,mocked-doi1,testUser,simple,OK
        |easy-dataset:2,.+,,,,-,FAILED: java.lang.Exception: easy-dataset:2
        |easy-dataset:3,.+,,,,-,FAILED: .*InvalidTransformationException: mocked
        |easy-dataset:4,.+,,mocked-doi4,testUser,simple,OK
        |""".stripMargin
      )
    outputDir.list.toSeq should have length 2
    outputDir.list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
    // the fatal bag is staged but not in the csv
    stagingDir.list.toSeq should have length 3
    stagingDir.list.toSeq.map(_.name)
      .count(csvContent.contains(_)) shouldBe 2
  }
}
