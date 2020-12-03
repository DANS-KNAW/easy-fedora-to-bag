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

import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture
import org.joda.time.DateTime

import scala.util.{ Failure, Success }

class VersionInfoSpec extends TestSupportFixture {
  "apply" should "return all types of identifiers" in {
    VersionInfo(
      <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
        <emd:date>
          <eas:dateSubmitted>20180223-01-01T00:10:34.000+01:00</eas:dateSubmitted>
        </emd:date>
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
      case Success(VersionInfo(
      date,
      Seq("urn:nbn:nl:ui:13-t3f-cz8", "10.17026/dans-zjf-522e", "easy-dataset:34340"),
      Seq("easy-dataset:123"),
      Seq("10.17026/dans-zjf-522e", "urn:nbn:nl:ui:13-2ajw-cq")
      )) if new DateTime(date) == new DateTime("2018-02-23T00:00:00.000+01:00") =>
    }
  }
  it should "report a missing dateSubmitted" in {
    VersionInfo(
      <emd:easymetadata>
        <emd:identifier>
          <dc:identifier>urn:nbn:nl:ui:13-t3f-cz8</dc:identifier>
        </emd:identifier>
      </emd:easymetadata>
    ) should matchPattern {
      case Failure(e) if e.getMessage == "Missing or invalid dateSubmitted []" =>
    }
  }
}
