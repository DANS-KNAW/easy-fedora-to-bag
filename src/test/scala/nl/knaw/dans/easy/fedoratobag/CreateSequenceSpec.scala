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

import java.io.{ IOException, StringWriter }

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import nl.knaw.dans.easy.fedoratobag.CsvRecord.csvFormat
import nl.knaw.dans.easy.fedoratobag.TransformationType.FEDORA_VERSIONED
import nl.knaw.dans.easy.fedoratobag.filter.{ FedoraVersionedFilter, InvalidTransformationException }
import nl.knaw.dans.easy.fedoratobag.fixture.{ DelegatingApp, FileSystemSupport, TestSupportFixture }

import scala.util.{ Failure, Success }

class CreateSequenceSpec extends TestSupportFixture with DelegatingApp with FileSystemSupport {

  private def outDir = {
    (testDir / "output").createDirectories()
  }

  private val options = Options(FedoraVersionedFilter(), FEDORA_VERSIONED, strict = false)

  "createSequences" should " process 2 sequences" in {
    val sw = new StringWriter()
    val createBagExpects = (1 to 5).map(i =>
      s"easy-dataset:$i" -> Success(DatasetInfo(None, s"mocked-doi$i", "mocked-urn", "user001", withPayload = true))
    )
    // end of mocking

    val input =
      """easy-dataset:1,easy-dataset:2
        |easy-dataset:3,easy-dataset:4,easy-dataset:5
        |""".stripMargin.split("\n").iterator
    delegatingApp(testDir / "staging", createBagExpects)
      .createSequences(input, outDir, options)(csvFormat.print(sw)) shouldBe Success("no fedora/IO errors")

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      // manual check (with break point): the value of uuid1 repeats during a sequence
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:1,.+,,mocked-doi1,user001,fedora-versioned,OK
        |easy-dataset:2,.+,.+,mocked-doi2,user001,fedora-versioned,OK
        |easy-dataset:3,.+,,mocked-doi3,user001,fedora-versioned,OK
        |easy-dataset:4,.+,.+,mocked-doi4,user001,fedora-versioned,OK
        |easy-dataset:5,.+,.+,mocked-doi5,user001,fedora-versioned,OK
        |""".stripMargin
      )

    // all bags should be mentioned in csv

    (testDir / "output").list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )

    // three out of five bags should refer to the two others

    def getVersionOfUUID(file: File) = file
      .contentAsString.split("\n")
      .filter(_.startsWith("Is-Version-Of"))
      .map(_.replaceAll(".*:", ""))

    val bagInfos = (testDir / "output").listRecursively
      .filter(_.name == "bag-info.txt")
      .toSeq
    val versionOfUUIDs = bagInfos
      .flatMap(getVersionOfUUID)
      .distinct

    bagInfos should have size 5
    versionOfUUIDs should have size 2
    versionOfUUIDs.map(uuid =>
      (testDir / "output" / uuid).exists
    ) shouldBe Seq(true, true)
  }

  it should "not abort on a metadata rule violation" in {
    val sw = new StringWriter()
    val createBagExpects = Seq(
      "easy-dataset:1" -> Success(DatasetInfo(Some("Violates something"), "mocked-doi", "", "user001", withPayload = true)),
      "easy-dataset:10" -> Failure(InvalidTransformationException("Violates whatever")),
      // the two mocked results above mix a strict and non-strict run in a single test
      "easy-dataset:2" -> Success(DatasetInfo(None, "mocked-doi", "", "user001", withPayload = true)),
      "easy-dataset:3" -> Success(DatasetInfo(None, "mocked-doi", "", "user001", withPayload = true)),
      "easy-dataset:4" -> Failure(new FedoraClientException(404, "mocked not found")),
      "easy-dataset:5" -> Success(DatasetInfo(None, "mocked-doi", "", "user001", withPayload = true)),
      "easy-dataset:6" -> Failure(new IllegalArgumentException("mocked error")),
      "easy-dataset:8" -> Success(DatasetInfo(None, "mocked-doi", "", "user001", withPayload = true)),
      "easy-dataset:9" -> Failure(new IOException("mocked error")),
    )

    // end of mocking

    val input =
      """easy-dataset:10,easy-dataset:11,easy-dataset:12
        |easy-dataset:1,easy-dataset:2
        |easy-dataset:3,easy-dataset:4,easy-dataset:5
        |easy-dataset:6,easy-dataset:7
        |easy-dataset:8,easy-dataset:9
        |easy-dataset:12,easy-dataset:13
        |easy-dataset:14
        |""".stripMargin.split("\n").iterator
    delegatingApp(testDir / "staging", createBagExpects)
      .createSequences(input, outDir, options)(csvFormat.print(sw)) should matchPattern {
      case Failure(_: IOException) =>
    }

    // post conditions

    val csvContent = sw.toString
    csvContent should (fullyMatch regex
      // mocking allows to demonstrate the difference between a strict and non-strict run in a single test
      """easyDatasetId,uuid1,uuid2,doi,depositor,transformationType,comment
        |easy-dataset:10,,,,,-,FAILED: .*InvalidTransformationException: Violates whatever
        |easy-dataset:1,.+,,mocked-doi,user001,not strict fedora-versioned,Violates something
        |easy-dataset:2,.+,.+,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:3,.+,,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:4,.+,,,,-,FAILED: .*FedoraClientException: mocked not found
        |easy-dataset:5,.+,.+,mocked-doi,user001,fedora-versioned,OK
        |easy-dataset:6,,,,,-,FAILED: .*IllegalArgumentException.*
        |easy-dataset:8,.+,,mocked-doi,user001,fedora-versioned,OK
        |""".stripMargin
      )

    // The number of packages add up to 8 while the input has more datasets:
    // dataset 7 is ignored because the first dataset aborted the rest of the sequence
    // dataset 9 only ends up in the staging directory
    // further datasets are ignored because the batch aborted

    (testDir / "output").listRecursively.filter(_.name == "bag-info.txt")
      .toSeq should have size 5
    (testDir / "staging").listRecursively.filter(_.name == "bag-info.txt")
      .toSeq should have size 4

    // only completed bags should be mentioned in csv

    (testDir / "output").list.toSeq.map(_.name).foreach(packageId =>
      csvContent should include(packageId)
    )
    (testDir / "staging").list.toSeq.map(_.name).foreach(packageId =>
      csvContent shouldNot include(packageId)
    )
  }
}
