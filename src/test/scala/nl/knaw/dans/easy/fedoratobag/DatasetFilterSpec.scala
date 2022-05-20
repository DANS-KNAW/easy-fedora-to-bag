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

import com.typesafe.scalalogging.Logger
import nl.knaw.dans.easy.fedoratobag.filter.{ BagIndex, SimpleDatasetFilter, ThemaDatasetFilter }
import nl.knaw.dans.easy.fedoratobag.fixture.{ BagIndexSupport, EmdSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory
import org.slf4j.{ Logger => UnderlyingLogger }

import java.net.URI
import java.nio.file.Paths
import scala.util.Success
import scala.xml.Elem

class DatasetFilterSpec extends TestSupportFixture with BagIndexSupport with MockFactory with EmdSupport {

  private class MockedBagIndex extends BagIndex(new URI("http://localhost:20120/"))

  private val exportStates = "PUBLISHED,SUBMITTED".split(",").toList

  private val emdRights = <emd:rights>
                            <dct:accessRights eas:schemeId="common.dcterms.accessrights"
                                >REQUEST_PERMISSION</dct:accessRights>
                          </emd:rights>
  private val emdDoi = <emd:identifier>
                         <dc:identifier eas:identification-system="https://doi.org"
                                        eas:scheme="DOI"
                         >10.17026/test-Iiib-z9p-4ywa</dc:identifier>
                       </emd:identifier>

  "ThemaChecker.simpleViolations" should "accept thematische collectie" in {
    val emdTitle = <emd:title><dc:title xml:lang="nld">some thematische collectie</dc:title></emd:title>
    val emd = parseEmdContent(Seq(emdTitle, emdDoi, emdRights))

    themaChecker(loggerExpectsWarnings = Seq.empty)
          .violations(emd, emd2ddm(emd), amd("PUBLISHED"), List.empty, exportStates) shouldBe
      Success(None)
  }

  it should "report other titles" in {
    val emdTitle = <emd:title><dc:title xml:lang="nld">some collection</dc:title></emd:title>
    val emd = parseEmdContent(Seq(emdTitle, emdDoi, emdRights))

    themaChecker(loggerExpectsWarnings = Seq(
          "violated 3: invalid title some collection",
        )).violations(emd, emd2ddm(emd), amd("PUBLISHED"), List.empty, exportStates) shouldBe
      Success(Some("Violates 3: invalid title"))
  }

  it should "report mix of original and other files" in {
    val emdTitle = <emd:title><dc:title xml:lang="nld">no theme</dc:title></emd:title>
    val emd = parseEmdContent(Seq(emdTitle, emdDoi, emdRights))
    val fileInfos = List(
      "original/x.txt",
      "x.txt",
    ).map(p => new FileInfo("easy-file:2", Paths.get(p), "x.txt", 2, "text/plain", "ANONYMOUS", "ANONYMOUS", None, None, None, Paths.get(p)))

    simpleChecker(loggerExpectsWarnings = Seq(
          "violated 8: original and other files should not occur both",
        )).violations(emd, emd2ddm(emd), amd("PUBLISHED"), fileInfos, exportStates) shouldBe
      Success(Some("Violates 8: original and other files"))
  }

  it should "allow mix of original and other files" in {
    val emdTitle = <emd:title><dc:title xml:lang="nld">no theme</dc:title></emd:title>
    val emd = parseEmdContent(Seq(emdTitle, emdDoi, emdRights))
    val ddm = emd2ddm(emd)
    val fileInfos = List(
      "original/x.txt",
      "x.txt",
    ).map(p => new FileInfo("easy-file:2", Paths.get(p), "x.txt", 2, "text/plain", "ANONYMOUS", "ANONYMOUS", None, None, None, Paths.get(p)))

    SimpleDatasetFilter(allowOriginalAndOthers = true)
          .violations(emd, ddm, amd("PUBLISHED"), fileInfos, exportStates) shouldBe
      Success(None)
  }

  "SimpleChecker.simpleViolations" should "succeed" in {
    val emdTitle = <emd:title><dc:title xml:lang="nld">no theme</dc:title></emd:title>
    val emd = parseEmdContent(Seq(emdTitle, emdDoi, emdRights))
    simpleChecker(loggerExpectsWarnings = Seq(), mockBagIndexRespondsWith(body = "", code = 404)).violations(emd, emd2ddm(emd), amd("PUBLISHED"), List.empty, exportStates) shouldBe
      Success(None)
  }

  it should "report missing DOI" in {
    val emd = parseEmdContent(emdRights)
    simpleChecker(loggerExpectsWarnings = Seq(
          "violated 1: DANS DOI not found",
        ), bagIndex = null).violations(emd, emd2ddm(emd), amd("SUBMITTED"), List.empty, exportStates) shouldBe
      Success(Some("Violates 1: DANS DOI"))
  }

  it should "report thematische collectie" in {
    val emdTitle = <emd:title><dc:title xml:lang="nld">thematische collectie</dc:title></emd:title>
    val emd = parseEmdContent(Seq(emdTitle, emdDoi))

    simpleChecker(loggerExpectsWarnings = Seq(
          "violated 3: invalid title thematische collectie",
        )).violations(emd, emd2ddm(emd), amd("PUBLISHED"), List.empty, exportStates) shouldBe
      Success(Some("Violates 3: invalid title"))
  }

  it should "report invalid status" in {
    val emd = parseEmdContent(emdDoi)
    val exportStates = List("PUBLISHED")
    simpleChecker(loggerExpectsWarnings = Seq(
          "violated 5: invalid state SUBMITTED",
        )).violations(emd, emd2ddm(emd), amd("SUBMITTED"), List.empty, exportStates) shouldBe
      Success(Some("Violates 5: invalid state (SUBMITTED)"))
  }

  it should "report existing bag" in {
    val emd = parseEmdContent(Seq(emdDoi, emdRights))
    val result = "<bag-info><bag-id>blabla</bag-id><doi>10.80270/test-zwu-cxjx</doi></bag-info>"
    simpleChecker(
          loggerExpectsWarnings = Seq(s"violated 7: is in the vault $result"),
          mockBagIndexRespondsWith(body = s"<result>$result</result>", code = 200),
        ).violations(emd, emd2ddm(emd), amd("PUBLISHED"), List.empty, exportStates) shouldBe
      Success(Some("Violates 7: is in the vault"))
  }

  private def amd(state: String): Elem =
    <damd:administrative-md xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" version="0.1">
      <datasetState>{ state }</datasetState>
    </damd:administrative-md>

  private def simpleChecker(loggerExpectsWarnings: Seq[String],
                            bagIndex: BagIndex = mockBagIndexRespondsWith(body = "", code = 404)
                           ) = {
    val mockLogger = mock[UnderlyingLogger]
    (() => mockLogger.isWarnEnabled()) expects() anyNumberOfTimes() returning true
    loggerExpectsWarnings.foreach(s =>
      (mockLogger.warn(_: String)) expects s once()
    )

    new SimpleDatasetFilter(targetIndex = bagIndex) {
      override lazy val logger: Logger = Logger(mockLogger)
    }
  }

  private def themaChecker(loggerExpectsWarnings: Seq[String],
                           bagIndex: BagIndex = mockBagIndexRespondsWith(body = "", code = 404)
                          ) = {
    val mockLogger = mock[UnderlyingLogger]
    (() => mockLogger.isWarnEnabled()) expects() anyNumberOfTimes() returning true
    loggerExpectsWarnings.foreach(s =>
      (mockLogger.warn(_: String)) expects s once()
    )

    new ThemaDatasetFilter(targetIndex = bagIndex) {
      override lazy val logger: Logger = Logger(mockLogger)
    }
  }
}
