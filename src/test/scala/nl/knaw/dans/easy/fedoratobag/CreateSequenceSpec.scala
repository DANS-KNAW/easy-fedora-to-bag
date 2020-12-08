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
import java.net.URI

import better.files.File
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedoratobag.CsvRecord.csvFormat
import nl.knaw.dans.easy.fedoratobag.fixture.{ FileFoXmlSupport, FileSystemSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory

import scala.util.{ Success, Try }

class CreateSequenceSpec extends TestSupportFixture with MockFactory with FileFoXmlSupport with FileSystemSupport {

  /* delegate most of createBag to a mock to test the rest of the class and/or application */
  def delegatingApp(exportBagExpects: Seq[(String, Try[DatasetInfo])]): EasyFedoraToBagApp = new EasyFedoraToBagApp(
    new Configuration("testVersion", null, null, new URI(""), testDir / "staging", null)
  ) {
    // mock requires a constructor without parameters
    class MockEasyFedoraToBagApp() extends EasyFedoraToBagApp(null)

    private val delegate = mock[MockEasyFedoraToBagApp]
    exportBagExpects.foreach { case (id, result) =>
      (delegate.createBag(_: DatasetId, _: File, _: Options, _: Option[VersionInfo])
        ) expects(id, *, *, *) returning result
    }

    override def createBag(datasetId: DatasetId, bagDir: File, options: Options, firstVersionInfo: Option[VersionInfo] = None): Try[DatasetInfo] = {
      // mimic a part of the real method, the tested caller wants to move the bag
      DansV0Bag.empty(bagDir).map { bag =>
        firstVersionInfo.foreach(_.addVersionOf(bag))
        bag.save()
      }.getOrElse(s"mock of createBag failed for $datasetId")
      // mock the outcome of the method
      delegate.createBag(datasetId, bagDir, options, firstVersionInfo)
    }
  }

  private def outDir = {
    (testDir / "output").createDirectories()
  }

  "createSequences" should " process 2 sequences" in {
    val sw = new StringWriter()
    val exportBagExpects = (1 to 5).map(i =>
      s"easy-dataset:$i" -> Success(DatasetInfo(None, "mocked-doi", "mocked-urn", "user001", Seq.empty))
    )
    // end of mocking

    val input =
      """easy-dataset:1,easy-dataset:2
        |easy-dataset:3,easy-dataset:4,easy-dataset:5
        |""".stripMargin.split("\n").iterator
    delegatingApp(exportBagExpects)
      .createSequences(input, outDir)(csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      // the value of uuid1 repeats during a sequence
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:1,.*,,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:2,.*,.*,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:3,.*,,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:4,.*,.*,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:5,.*,.*,mocked-doi,user001,fedora-versioned,OK
        |""".stripMargin
      )
    // TODO testDir/output/*/*/bag-info.txt should have 5 bags,
    //  3 of them should have versionOf values pointing to the other 2
  }

  it should "not abort on a metadata rule violation" in {
    val sw = new StringWriter()
    val exportBagExpects = (2 to 5).map(i =>
      s"easy-dataset:$i" -> Success(DatasetInfo(None, "mocked-doi", "", "user001", Seq.empty))
    ) :+ ("easy-dataset:1" -> Success(DatasetInfo(Some("Violates something"), "mocked-doi", "", "user001", Seq.empty)))

    // end of mocking

    val input =
      """easy-dataset:1,easy-dataset:2
        |easy-dataset:3,easy-dataset:4,easy-dataset:5
        |""".stripMargin.split("\n").iterator
    delegatingApp(exportBagExpects)
      .createSequences(input, outDir)(csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post conditions

    val csvContent = sw.toString
    // the value of uuid1 repeats during a sequence
    csvContent should (fullyMatch regex
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:1,.*,,mocked-doi,user001,not strict fedora-versioned,Violates something
        |easy-dataset:2,.*,OK
        |easy-dataset:3,.*,OK
        |easy-dataset:4,.*,OK
        |easy-dataset:5,.*,OK
        |""".stripMargin
      )
    // TODO skip a sequence on a failing first bag
    //  skip a single version on a failure
  }
  it should "abort the batch on a fedora/io/not-expected exception" in {
    // TODO
  }
}
