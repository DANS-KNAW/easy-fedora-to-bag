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

import java.net.URI

import nl.knaw.dans.easy.fedora2vault.fixture.{ EmdSupport, TestSupportFixture }
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.scalamock.scalatest.MockFactory

import scala.util.{ Failure, Success }

class SimpleCheckerSpec extends TestSupportFixture with MockFactory with EmdSupport {
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  private class MockedBagIndex extends BagIndex(new URI("http://localhost:20120/"))

  "isSimple" should "succeed" in {
    val emd = parseEmdContent(
      <emd:title><dc:title xml:lang="nld"
      >no theme</dc:title></emd:title>
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
      <emd:rights><dct:accessRights eas:schemeId="common.dcterms.accessrights"
      >REQUEST_PERMISSION</dct:accessRights></emd:rights>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex once() returning Success(None)

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("PUBLISHED"), Seq.empty) shouldBe
      Success(())
  }

  it should "report missing DOI" in {
    val emd = parseEmdContent(
      <emd:rights><dct:accessRights eas:schemeId="common.dcterms.accessrights"
      >REQUEST_PERMISSION</dct:accessRights></emd:rights>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex never()

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("SUBMITTED"), Seq.empty) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: no DOI" =>
    }
  }

  it should "report thematische collectie" in {
    val emd = parseEmdContent(
      <emd:title><dc:title xml:lang="nld"
      >thematische collectie</dc:title></emd:title>
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex never()

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("PUBLISHED"), Seq()) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: is a thematische collectie" =>
    }
  }

  it should "report jump off" in {
    val emd = parseEmdContent(
      <emd:title><dc:title xml:lang="nld"
      >thematische collectie</dc:title></emd:title>
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex never()

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("PUBLISHED"), Seq("easy-jumpoff:123")) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: has easy-jumpoff:123" =>
    }
  }

  it should "report invalid status" in {
    val emd = parseEmdContent(
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex never()

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("SUBMITTED"), Seq.empty) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: AccessCategory is neither OPEN_ACCESS nor REQUEST_PERMISSION" =>
    }
  }

  it should "report not published" in {
    val emd = parseEmdContent(
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
      <emd:rights><dct:accessRights eas:schemeId="common.dcterms.accessrights"
      >REQUEST_PERMISSION</dct:accessRights></emd:rights>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex never()

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("SUBMITTED"), Seq.empty) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: not published" =>
    }
  }

  it should "report invalid relations" in {
    val emd = parseEmdContent(
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
    <emd:relation>
        <dct:isVersionOf>https://doi.org/10.17026/test-123-456</dct:isVersionOf>
        <dct:isVersionOf>http://www.persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-2ajw-cq</dct:isVersionOf>
        <eas:replaces>
            <eas:subject-title>Prehistorische bewoning op het World Forum gebied - Den Haag (replaces)</eas:subject-title>
            <eas:subject-identifier eas:scheme="BID1" eas:identification-system="http://pid.org/sys1">ABC1</eas:subject-identifier>
            <eas:subject-link>http://persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-aka-hff</eas:subject-link>
        </eas:replaces>
    </emd:relation>
      <emd:rights><dct:accessRights eas:schemeId="common.dcterms.accessrights"
      >REQUEST_PERMISSION</dct:accessRights></emd:rights>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex never()

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("PUBLISHED"), Seq.empty) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: not published" =>
    }
  }

  it should "report existing bag" in {
    val emd = parseEmdContent(
      <emd:identifier><dc:identifier eas:identification-system="https://doi.org"
                                     eas:scheme="DOI"
      >10.17026/test-Iiib-z9p-4ywa</dc:identifier></emd:identifier>
      <emd:rights><dct:accessRights eas:schemeId="common.dcterms.accessrights"
      >REQUEST_PERMISSION</dct:accessRights></emd:rights>
    )
    implicit val mockedBagIndex: MockedBagIndex = mock[MockedBagIndex]
    expectBagIndex once() returning Success(Some("---"))

    SimpleChecker(mockedBagIndex)
      .isSimple(emd, emd2ddm(emd), amd("PUBLISHED"), Seq.empty) should matchPattern {
      case Failure(t: Throwable) if t.getMessage == "Not a simple dataset: Dataset found in vault. DOI[10.17026/test-Iiib-z9p-4ywa] http://localhost:20120/ returned: ---" =>
    }
  }

  private def expectBagIndex(implicit mockedBagIndex: MockedBagIndex) = {
    (mockedBagIndex.bagByDoi(_: String)) expects *
  }

  private def amd(state: String) =
    <damd:administrative-md xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" version="0.1">
      <datasetState>{ state }</datasetState>
    </damd:administrative-md>
}
