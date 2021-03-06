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

import nl.knaw.dans.easy.fedoratobag.filter._
import nl.knaw.dans.easy.fedoratobag.fixture.{ FileFoXmlSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory

import java.nio.file.Paths
import scala.util.Success
import scala.xml.NodeBuffer

class FileInfosSpec extends TestSupportFixture with FileFoXmlSupport with MockFactory {
  private val fileInfo = new FileInfo("easy-file:1", Paths.get("x.txt"), "x.txt", size = 2, mimeType = "text/plain", accessibleTo = "ANONYMOUS", visibleTo = "ANONYMOUS", contentDigest = None, additionalMetadata = None)
  "selectForXxxBag" should "return files for two bags" in {
    val fileInfos = List(
      fileInfo.copy(fedoraFileId = "easy-file:1", path = Paths.get("original/x.txt")),
      fileInfo.copy(fedoraFileId = "easy-file:2"),
    )
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = false)
    for2nd shouldBe fileInfos
    for1st shouldBe Success(fileInfos.slice(0, 1))
  }
  it should "return one file for each bag" in {
    val fileInfos = List(
      fileInfo.copy(fedoraFileId = "easy-file:1", path = Paths.get("original/x.txt"), accessibleTo = "NONE", visibleTo = "NONE"),
      fileInfo.copy(fedoraFileId = "easy-file:2"),
    )
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = false)
    for2nd shouldBe fileInfos.slice(1, 2)
    for1st shouldBe Success(fileInfos.slice(0, 1))
    // identical files are filtered
  }
  it should "return files for one bag" in {
    val fileInfos = List(
      fileInfo.copy(fedoraFileId = "easy-file:1", path = Paths.get("rabarbera/x.txt")),
      fileInfo.copy(fedoraFileId = "easy-file:2"),
    )
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = false)
    for2nd shouldBe empty
    for1st shouldBe Success(fileInfos)
  }
  it should "return no files" in {
    val fileInfos = List(
      fileInfo.copy(fedoraFileId = "easy-file:1", path = Paths.get("rabarbera/x.txt")),
      fileInfo.copy(fedoraFileId = "easy-file:2"),
    )
    // though commandline does not allow isOriginalVersioned nor europeana
    // together with noPayload, the latter gets highest priority
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true, noPayload = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = true, noPayload = true)
    for2nd shouldBe empty
    for1st shouldBe Success(Seq.empty)
  }
  "FileInfo" should "replace non allowed characters in name and filepath with '_'" in {
    val foxml = fileFoXml(
      location = "p:t*/t?/s>m|w;e#e",
      name = "a:c*e?g>i|k;m#o\".txt",
    )
    val fedoraProvider = mock[FedoraProvider]
    (fedoraProvider.loadFoXml(_: String)) expects "easy-file:35" once() returning Success(foxml)

    val fileInfo = FileInfo(List("easy-file:35"), fedoraProvider)
      .getOrElse(fail("could not load test data")).head
    fileInfo.name shouldBe "a_c_e_g_i_k_m_o_.txt"
    fileInfo.path shouldBe Paths.get("p_t_/t_/s_m_w_e_e/a_c_e_g_i_k_m_o_.txt")
  }
}
