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

import java.nio.file.Paths
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

class FileInfoSpec  extends TestSupportFixture {

  "equals" should "classify equal digests as equal FileInfos" in {
    val firstFileInfo = new FileInfo("easy-file:1", Paths.get("dir/file1"), "file1", 100.1, "application/pdf", "ANONYMOUS", "ANONYMOUS", Option(<foxml:contentDigest TYPE="SHA-1" DIGEST="5ed8868fe6ce598f1ef8839d5cb188245538b16c"/>), None)
    val secondFileInfo = new FileInfo("easy-file:2", Paths.get("dir/file1"), "file1", 100.1, "application/pdf", "NONE", "ANONYMOUS", Option(<foxml:contentDigest TYPE="SHA-1" DIGEST="5ed8868fe6ce598f1ef8839d5cb188245538b16c"/>), None)

    firstFileInfo.equals(secondFileInfo) shouldBe true
  }

  it should "classify FileInfo without digest with the same path as equal FileInfos" in {
    val firstFileInfo = new FileInfo("easy-file:1", Paths.get("dir/file1"), "file1", 100.1, "application/pdf", "ANONYMOUS", "ANONYMOUS", None, None)
    val secondFileInfo = new FileInfo("easy-file:2", Paths.get("dir/file1"), "file1", 100.1, "application/pdf", "ANONYMOUS", "ANONYMOUS", None, None)

    firstFileInfo.equals(secondFileInfo) shouldBe true
  }

  it should "make a set unique" in {
    val firstFileInfo = new FileInfo("easy-file:1", Paths.get("dir/file1"), "file1", 100.1, "application/pdf", "ANONYMOUS", "ANONYMOUS", Option(<foxml:contentDigest TYPE="SHA-1" DIGEST="5ed8868fe6ce598f1ef8839d5cb188245538b16c"/>), None)
    val secondFileInfo = new FileInfo("easy-file:2", Paths.get("dir/file1"), "file1", 100.1, "application/pdf", "ANONYMOUS", "ANONYMOUS", Option(<foxml:contentDigest TYPE="SHA-1" DIGEST="5ed8868fe6ce598f1ef8839d5cb188245538b16c"/>), None)

    List(firstFileInfo, secondFileInfo).distinct.size shouldBe 1
  }
}
