package nl.knaw.dans.easy.fedora2vault

import java.net.UnknownHostException

import better.files.StringExtensions
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.fedora2vault.fixture.TestSupportFixture

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, SAXParseException, Utility, XML }

class DdmSpec extends TestSupportFixture {

  private val emdNS = "http://easy.dans.knaw.nl/easy/easymetadata/"
  private val easNS = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"
  private val dctNS = "http://purl.org/dc/terms/"
  private val dcNS = "http://purl.org/dc/elements/1.1/"
  private lazy val triedSchema = Try(SchemaFactory
    .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    .newSchema(Array(new StreamSource("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd")).toArray[Source])
  )

  "TalkOfEurope" should "get a DDM out of its EMD" in {
    val triedString = FoXml
      .getEmd(XML.loadFile((samples / "TalkOfEurope.xml").toJava))
      .flatMap(DDM(_).map(toS))
    triedString.map(strip) shouldBe Success(
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
    assume(schemaIsAvailable)
    triedString.flatMap(validate) shouldBe a[Success[_]]
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
      </emd:easymetadata>
    ).map(toStripped)
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
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
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

  it should "tolerate an empty EMD" in {
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <ddm:accessRights/>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata/>
        |</ddm:DDM>
        |""".stripMargin)
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
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <created>03-2013</created>
        |    <created xsi:type="W3CDTF">1901-04</created>
        |    <available>04-2013</available>
        |    <available xsi:type="W3CDTF">1900</available>
        |    <available xsi:type="W3CDTF">1902-04</available>
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

  private def toStripped(elem: Elem) = strip(toS(elem))

  private def toS(elem: Elem) = printer.format(Utility.trim(elem))

  private def strip(s: String) = s
    .replaceAll(nameSpaceRegExp, "")
    .replaceAll(" \n", "\n")

  private def validate(serialized: String) = {
    triedSchema.flatMap { schema =>
      val source = new StreamSource(serialized.inputStream)
      Try(schema.newValidator().validate(source))
    }
  }

  private def schemaIsAvailable = {
    triedSchema match {
      case Failure(e: SAXParseException) if e.getCause.isInstanceOf[UnknownHostException] => false
      case Failure(e: SAXParseException) if e.getMessage.contains("Cannot resolve") =>
        println("Probably an offline third party schema: " + e.getMessage)
        false
      case _ => true
    }
  }
}
