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
package nl.knaw.dans.easy.fedoratobag.versions

import better.files.File
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture
import org.joda.time.DateTime

import scala.reflect.runtime.universe.Try
import scala.util.Success
import scala.xml.XML

class EmdVersionSpec extends TestSupportFixture {
  "apply" should "return all types of identifiers" in {
    EmdVersionInfo(
      <emd:easymetadata xmlns:eas={ EmdVersionInfo.easNameSpace }>
        <emd:identifier>
          <dc:identifier eas:scheme="PID">urn:nbn:nl:ui:13-t3f-cz8</dc:identifier>
          <dc:identifier eas:scheme="DOI">10.17026/dans-zjf-522e</dc:identifier>
          <dc:identifier eas:scheme="eDNA-project">a12893</dc:identifier>
          <dc:identifier>bcdef</dc:identifier>
          <dc:identifier eas:scheme="DMO_ID">easy-dataset:34340</dc:identifier>
        </emd:identifier>
        <emd:relation>
          <eas:replaces>
            <eas:subject-title>easy-dataset:123</eas:subject-title>
            <eas:subject-link/>
          </eas:replaces>
          <eas:hasVersion>
            <eas:subject-title>Plangebied Elspeet Noord IVO3.</eas:subject-title>
            <eas:subject-link>https://doi.org/10.17026/dans-zjf-522e</eas:subject-link>
          </eas:hasVersion>
          <dct:hasVersion>http://www.persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-2ajw-cq</dct:hasVersion>
        </emd:relation>
      </emd:easymetadata>
    ) should matchPattern {
      case Success(EmdVersionInfo(
      _,
      Seq("urn:nbn:nl:ui:13-t3f-cz8", "10.17026/dans-zjf-522e", "easy-dataset:34340"),
      Seq("easy-dataset:123"),
      Seq("10.17026/dans-zjf-522e", "urn:nbn:nl:ui:13-2ajw-cq")
      )) =>
    }
  }
  it should "use a default Date" in {
    EmdVersionInfo(<emd:easymetadata/>)
      .map(submitYear) shouldBe Success(1900)
  }
  it should "fall back to a default Date" in {
    EmdVersionInfo(
      <emd:easymetadata>
        <emd:date><eas:dateSubmitted>blablabla</eas:dateSubmitted></emd:date>
      </emd:easymetadata>
    ).map(submitYear) shouldBe Success(1970)
  }
  it should "use dateSubmitted" in {
    EmdVersionInfo(
      <emd:easymetadata>
        <emd:date><eas:dateCreated>1980</eas:dateCreated></emd:date>
        <emd:date><eas:dateSubmitted>1990</eas:dateSubmitted></emd:date>
      </emd:easymetadata>
    ).map(submitYear) shouldBe Success(1990)
  }
  it should "fall back to date created" in {
    EmdVersionInfo(
      <emd:easymetadata>
        <emd:date><eas:date>1980</eas:date></emd:date>
        <emd:date><dc:created>1980</dc:created></emd:date>
      </emd:easymetadata>
    ).map(submitYear) shouldBe Success(1980)
  }

  private def submitYear(l: EmdVersionInfo) = {
    new DateTime(l.submitted).getYear
  }
}
