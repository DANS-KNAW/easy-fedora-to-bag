package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.easy.fedoratobag.filter.Versions
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

import scala.util.Success

class VersionsSpec extends TestSupportFixture {
  "apply" should "" in {
    Versions(
      <emd:easymetadata>
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
    ) shouldBe Success(new Versions(Seq("easy-dataset:123"),Seq("10.17026/dans-zjf-522e", "urn:nbn:nl:ui:13-2ajw-cq")))
  }
}
