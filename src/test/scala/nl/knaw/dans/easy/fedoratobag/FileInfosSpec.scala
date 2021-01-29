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
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

import java.nio.file.Paths
import scala.util.Success

class FileInfosSpec extends TestSupportFixture {
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
}
