package nl.knaw.dans.easy.fedora2vault

import nl.knaw.dans.easy.fedora2vault.fixture.TestSupportFixture

import scala.util.Success
import scala.xml.XML

class DdmSpec extends TestSupportFixture {
  private val other =
              <emd:other>
              <eas:application-specific>
                  <eas:metadataformat>ANY_DISCIPLINE</eas:metadataformat>
                  <eas:pakbon-status>NOT_IMPORTED</eas:pakbon-status>
              </eas:application-specific>
              <eas:etc/>
          </emd:other>

  "TalkOfEurope" should "get a DDM out of its EMD" in {
    val triedString = FoXml.getEmd(XML.loadFile((samples / "TalkOfEurope.xml").toJava))
      .flatMap(DDM(_).map(_.serialize))
    triedString shouldBe a[Success[_]]
    // compare content in parts to get rid of prologue and namespace clutter
    triedString.get should include
          <ddm:profile>
            <dc:title xml:lang="eng">Talk of Europe - The debates of the European Parliament as Linked Open Data</dc:title>
            <dcterms:description xml:lang="eng">
              The Talk of Europe (TOE) project has curated the proceedings of the European Parliament (EP) from 1999 onwards, including all available translations in other EU languages, and converted these to RDF. Moreover, the data are enriched with biographical and political information on the speakers. Since the data are available in multilingual form, this dataset lends itself to be linked with resources in other European countries, such as parliamentary records or news reports.
            </dcterms:description>
            <ddm:created>2015-12-31</ddm:created>
            <ddm:available>2015-12-31</ddm:available>
            <ddm:audience>easy-discipline:42</ddm:audience>
            <ddm:audience>easy-discipline:11</ddm:audience>
            <ddm:audience>easy-discipline:14</ddm:audience>
            <ddm:audience>easy-discipline:6</ddm:audience>
            <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
          </ddm:profile>.serialize
    triedString.get should include
      <ddm:dcmiMetadata>
        <dcterms:identifier xsi:type="PID">urn:nbn:nl:ui:13-3ax2-te</dcterms:identifier>
        <dcterms:identifier xsi:type="DOI">10.17026/test-dans-2xg-umq8</dcterms:identifier>
        <dcterms:identifier>TalkOfEurope</dcterms:identifier>
        <dcterms:identifier xsi:type="DMO_ID"/>
      </ddm:dcmiMetadata>.serialize

  }
  "dates" should "convert" in {
    // dates from samples/TalkOfEurope.xml + easy-schema-examples/src/main/resources/examples/ddm/example2.xml
    val triedString = DDM(
      <emd:easymetadata xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/" xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/" xmlns:dct="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" emd:version="0.1">
          <emd:date>
              <dc:date>11-2013</dc:date>
              <dc:date>12-2013</dc:date>
              <dct:created>03-2013</dct:created>
              <dct:valid>06-2013</dct:valid>
              <dct:available>04-2013</dct:available>
              <dct:issued>07-2013</dct:issued>
              <dct:modified>08-2013</dct:modified>
              <dct:dateAccepted>05-2013</dct:dateAccepted>
              <dct:dateCopyrighted>09-2013</dct:dateCopyrighted>
              <dct:dateSubmitted>10-2013</dct:dateSubmitted>
              <eas:date eas:scheme="W3CDTF" eas:format="MONTH">1909-04-01T00:00:00.000+00:19:32</eas:date>
              <eas:date eas:scheme="W3CDTF" eas:format="MONTH">1910-04-01T00:00:00.000+00:19:32</eas:date>
              <eas:created eas:scheme="W3CDTF" eas:format="MONTH">1901-04-01T00:00:00.000+00:19:32</eas:created>
              <eas:valid eas:scheme="W3CDTF" eas:format="MONTH">1904-04-01T00:00:00.000+00:19:32</eas:valid>
              <eas:available eas:scheme="W3CDTF" eas:format="YEAR">1900-01-01T00:00:00.000+00:19:32</eas:available>
              <eas:available eas:scheme="W3CDTF" eas:format="MONTH">1902-04-01T00:00:00.000+00:19:32</eas:available>
              <eas:issued eas:scheme="W3CDTF" eas:format="MONTH">1905-04-01T00:00:00.000+00:19:32</eas:issued>
              <eas:modified eas:scheme="W3CDTF" eas:format="MONTH">1906-04-01T00:00:00.000+00:19:32</eas:modified>
              <eas:dateAccepted eas:scheme="W3CDTF" eas:format="MONTH">1903-04-01T00:00:00.000+00:19:32</eas:dateAccepted>
              <eas:dateCopyrighted eas:scheme="W3CDTF" eas:format="MONTH">1907-04-01T00:00:00.000+00:19:32</eas:dateCopyrighted>
              <eas:dateSubmitted eas:scheme="W3CDTF" eas:format="MONTH">1908-04-01T00:00:00.000+00:19:32</eas:dateSubmitted>
          </emd:date>
        { other }
      </emd:easymetadata>
    ).map(_.serialize)
    triedString shouldBe a[Success[_]]
    triedString.get should include
      <ddm:profile>
        <ddm:created>2015-12-31</ddm:created>
        <ddm:available>2015-12-31</ddm:available>
      </ddm:profile>.serialize
    triedString.get should include
      <ddm:dcmiMetadata>
      </ddm:dcmiMetadata>.serialize
  }
}
