package nl.knaw.dans.easy.fedora2vault

import nl.knaw.dans.easy.fedora2vault.fixture.TestSupportFixture

import scala.util.{ Failure, Success }
import scala.xml.{ Elem, XML }

class DdmSpec extends TestSupportFixture {

  private val emdNS = "http://easy.dans.knaw.nl/easy/easymetadata/"
  private val easNS = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"
  private val dctNS = "http://purl.org/dc/terms/"
  private val dcNS = "http://purl.org/dc/elements/1.1/"
  private val other =
          <emd:other>
              <eas:application-specific>
                  <eas:metadataformat>ANY_DISCIPLINE</eas:metadataformat>
                  <eas:pakbon-status>NOT_IMPORTED</eas:pakbon-status>
              </eas:application-specific>
              <eas:etc/>
          </emd:other>

  "TalkOfEurope" should "get a DDM out of its EMD" in {
    FoXml.getEmd(XML.loadFile((samples / "TalkOfEurope.xml").toJava))
      .flatMap(DDM(_).map(toString)) shouldBe Success(
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <dc:title xml:lang="eng">Talk of Europe - The debates of the European Parliament as Linked Open Data</dc:title>
        |    <dcterms:description xml:lang="eng">
        |      The Talk of Europe (TOE) project has curated the proceedings of the European Parliament (EP) from 1999 onwards, including all available translations in other EU languages, and converted these to RDF. Moreover, the data are enriched with biographical and political information on the speakers. Since the data are available in multilingual form, this dataset lends itself to be linked with resources in other European countries, such as parliamentary records or news reports.
        |    </dcterms:description>
        |    <created xsi:type="W3CDTF">2015-12-31</created>
        |    <available xsi:type="W3CDTF">2015-12-31</available>
        |    <ddm:audience>easy-discipline:42</ddm:audience>
        |    <ddm:audience>easy-discipline:11</ddm:audience>
        |    <ddm:audience>easy-discipline:14</ddm:audience>
        |    <ddm:audience>easy-discipline:6</ddm:audience>
        |    <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata>
        |    <dcterms:identifier xsi:type="PID">urn:nbn:nl:ui:13-3ax2-te</dcterms:identifier>
        |    <dcterms:identifier xsi:type="DOI">10.17026/test-dans-2xg-umq8</dcterms:identifier>
        |    <dcterms:identifier>TalkOfEurope</dcterms:identifier>
        |    <dcterms:identifier xsi:type="DMO_ID"></dcterms:identifier>
        |    <dateSubmitted xsi:type="W3CDTF">2015-10-14</dateSubmitted>
        |  </ddm:dcmiMetadata>
        |</ddm:DDM>
        |""".stripMargin)
  }

  "descriptions" should "..." in {
    // more than easy-ddm/src/test/resources/ddm2emdCrosswalk/ddmDescriptionWithRequiredDescriptionType.input.xml
    // TODO EMD:remarks to <ddm:description descriptionType="TechnicalInfo">
    val triedString = DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        <emd:description>
          <dc:description>abstract</dc:description>
          <dc:description>Suggestions for data usage: remark1</dc:description>
          <dc:description>beschrijving</dc:description>
          <dct:tableOfContents>rabar</dct:tableOfContents>
          <dct:abstract>blabl</dct:abstract>
        </emd:description>
        <emd:date>
          <dct:created>03-2013</dct:created>
        </emd:date>
        { other }
      </emd:easymetadata>
    ).map(toString)
    triedString shouldBe Success(
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <dcterms:description>abstract</dcterms:description>
        |    <dcterms:description>Suggestions for data usage: remark1</dcterms:description>
        |    <dcterms:description>beschrijving</dcterms:description>
        |    <dcterms:description descriptionType="Abstract">blabl</dcterms:description>
        |    <dcterms:description descriptionType="TableOfContent">rabar</dcterms:description>
        |    <created>03-2013</created>
        |    <available>03-2013</available>
        |    <ddm:accessRights/>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata/>
        |</ddm:DDM>
        |""".stripMargin)
  }

  "dates" should "use created for available" in {
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
          <emd:date>
              <dct:created>03-2013</dct:created>
          </emd:date>
        { other }
      </emd:easymetadata>
    ).map(toString) shouldBe Success(
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <created>03-2013</created>
        |    <available>03-2013</available>
        |    <ddm:accessRights/>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata/>
        |</ddm:DDM>
        |""".stripMargin)
  }

  it should "fail without a date created" in {
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        { other }
      </emd:easymetadata>
    ).map(toString) shouldBe a[Failure[_]]
  }

  it should "render only the first available" in {
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
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
    ).map(toString) shouldBe Success(
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <created>03-2013</created>
        |    <created xsi:type="W3CDTF">1901-04</created>
        |    <available>04-2013</available>
        |    <ddm:accessRights/>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata>
        |    <date>11-2013</date>
        |    <date>12-2013</date>
        |    <date xsi:type="W3CDTF">1909-04</date>
        |    <date xsi:type="W3CDTF">1910-04</date>
        |    <dateCopyrighted>09-2013</dateCopyrighted>
        |    <dateCopyrighted xsi:type="W3CDTF">1907-04</dateCopyrighted>
        |    <dateSubmitted>10-2013</dateSubmitted>
        |    <dateSubmitted xsi:type="W3CDTF">1908-04</dateSubmitted>
        |    <modified>08-2013</modified>
        |    <modified xsi:type="W3CDTF">1906-04</modified>
        |    <issued>07-2013</issued>
        |    <issued xsi:type="W3CDTF">1905-04</issued>
        |    <dateAccepted>05-2013</dateAccepted>
        |    <dateAccepted xsi:type="W3CDTF">1903-04</dateAccepted>
        |    <valid>06-2013</valid>
        |    <valid xsi:type="W3CDTF">1904-04</valid>
        |  </ddm:dcmiMetadata>
        |</ddm:DDM>
        |""".stripMargin)
  }

  private def toString(elem: Elem) = {
    elem.serialize
      .replaceAll(nameSpaceRegExp, "")
      .replaceAll(" \n", "\n")
      .replaceAll(".*[?]>\n", "")
  }
}
