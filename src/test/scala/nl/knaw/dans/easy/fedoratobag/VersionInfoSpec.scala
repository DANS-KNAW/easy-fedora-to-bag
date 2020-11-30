package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.easy.fedoratobag.filter.VersionInfo
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

import scala.util.Success

class VersionInfoSpec extends TestSupportFixture {
  "apply" should "" in {
    VersionInfo(
      <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
        <emd:date>
          <eas:dateSubmitted eas:scheme="W3CDTF" eas:format="DAY">2010-07-30T00:00:00.000+02:00</eas:dateSubmitted>
        </emd:date>
        <emd:identifier>
          <dc:identifier eas:scheme="PID" eas:identification-system="http://www.persistent-identifier.nl">urn:nbn:nl:ui:13-t3f-cz8</dc:identifier>
          <dc:identifier eas:scheme="DOI" eas:identification-system="http://dx.doi.org">10.17026/dans-zjf-522e</dc:identifier>
          <dc:identifier eas:scheme="eDNA-project">a12893</dc:identifier>
          <dc:identifier eas:scheme="DMO_ID">easy-dataset:34340</dc:identifier>
        </emd:identifier>
        <emd:relation>
          <eas:replaces>
            <eas:subject-title>easy-dataset:123</eas:subject-title>
            <eas:subject-link/>
          </eas:replaces>
          <eas:hasVersion>
            <eas:subject-title>ADC ArcheoProjecten; Huisman, N. (2009): Nunspeet Plangebied Elspeet Noord IVO3.</eas:subject-title>
            <eas:subject-link>https://doi.org/10.17026/dans-zjf-522e</eas:subject-link>
          </eas:hasVersion>
          <dct:hasVersion>http://www.persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-2ajw-cq</dct:hasVersion>
        </emd:relation>
      </emd:easymetadata>
    ) should matchPattern {
      case Success(VersionInfo(
      _,
      Seq("urn:nbn:nl:ui:13-t3f-cz8", "10.17026/dans-zjf-522e", "easy-dataset:34340"),
      Seq("easy-dataset:123"),
      Seq("10.17026/dans-zjf-522e", "urn:nbn:nl:ui:13-2ajw-cq")
      )) =>
    }
  }
}
