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

import java.net.UnknownHostException

import better.files.{ File, StringExtensions }
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.fedora2vault.fixture.{ AudienceSupport, EmdSupport, TestSupportFixture }
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller

import scala.util.{ Failure, Success, Try }
import scala.xml.Utility.trim
import scala.xml._

class DdmSpec extends TestSupportFixture with EmdSupport with AudienceSupport {

  private lazy val triedSchema = Try(SchemaFactory
    .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    .newSchema(Array(new StreamSource("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd")).toArray[Source])
  )
  private val emdUnMarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])
  private val emdTitle =
        <emd:title>
            <dc:title>XXX</dc:title>
        </emd:title>
  private val emdCreator =
        <emd:creator>
            <eas:creator>
                <eas:organization>DANS</eas:organization>
                <eas:entityId eas:scheme="DAI"></eas:entityId>
            </eas:creator>
        </emd:creator>
  val ddmCreator = // resulting from emdCreator
    """    <dcx-dai:creatorDetails>
      |      <dcx-dai:organization>
      |        <dcx-dai:name>DANS</dcx-dai:name>
      |      </dcx-dai:organization>
      |    </dcx-dai:creatorDetails>"""
  private val emdDescription =
        <emd:description>
            <dc:description>YYY</dc:description>
        </emd:description>
  private val emdDates =
        <emd:date>
            <eas:created eas:scheme="W3CDTF" eas:format="DAY">2017-09-30T00:00:00.000+02:00</eas:created>
            <eas:available eas:scheme="W3CDTF" eas:format="DAY">2017-09-30T17:47:36.978+02:00</eas:available>
        </emd:date>
  private val emdRights =
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dcterms.accessrights">OPEN_ACCESS</dct:accessRights>
        </emd:rights>

  "streaming" should "get a valid DDM out of its EMD" in {
    val file = "streaming.xml"
    val triedDdm = getEmd(file).flatMap(DDM(_, Seq("D35400")))
    triedDdm.map(toS).map(normalize(_)
      .split("\n") // TODO dropping a line that would not validate
      .filterNot(_.contains("""<dct:relation xsi:type="id-type:STREAMING_SURROGATE_RELATION">"""))
      .mkString("\n")
    ) shouldBe Success(expectedDDM(file).trim)
  }

  "depositApi" should "produce the DDM provided by easy-deposit-api" in {
    val triedFoXml = Try(XML.loadFile((sampleFoXML / "DepositApi.xml").toJava))
    val triedDdm = getEmd("DepositApi.xml")
      .flatMap(DDM(_, Seq("D13200")))
      .map(toS)
    triedDdm shouldBe a[Success[_]]

    // round trip test (foXml/EMD was created from the foXML/DDM by easy-ingest-flow)
    triedDdm.map(normalize) shouldBe triedFoXml.map(foXml =>
      normalize(toS((foXml \\ "DDM").head))
        .replaceAll("dcterms:", "dct:")
        .replaceAll("""<dcx-dai:name xml:lang="nld">""", """<dcx-dai:name>""") // TODO api bug? lang on title?
    )
    validate(triedDdm) shouldBe Success(())
  }

  "descriptions" should "all appear" in {
    val emd = parseEmdContent(Seq(
        <emd:description>
          <dc:description>abstract</dc:description>
          <dc:description>Suggestions for data usage: remark1</dc:description>
          <dc:description>beschrijving</dc:description>
          <dct:tableOfContents>rabar</dct:tableOfContents>
          <dct:abstract>blabl</dct:abstract>
        </emd:description>
    ))
    DDM(emd, Seq.empty).map(trim) shouldBe Success(trim(
      <ddm:DDM xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         <ddm:profile>
           <dct:description>abstract</dct:description>
           <dct:description>Suggestions for data usage: remark1</dct:description>
           <dct:description>beschrijving</dct:description>
           <ddm:accessRights/>
         </ddm:profile>
         <ddm:dcmiMetadata>
           <ddm:description descriptionType="Abstract">blabl</ddm:description>
           <ddm:description descriptionType="TableOfContent">rabar</ddm:description>
           <dct:license xsi:type="dct:URI">{ DDM.dansLicense }</dct:license>
         </ddm:dcmiMetadata>
       </ddm:DDM>
     ))
  }

  "relations" should "all appear" in {
    val emd = parseEmdContent(Seq(
        <emd:relation>
          <dct:hasVersion eas:scheme="ISSN">my-issn-related-identifier</dct:hasVersion>
          <dct:requires eas:scheme="ISBN">my-isbn-related-identifier</dct:requires>
          <dct:isPartOf>my own related identifier</dct:isPartOf>
          <dct:isFormatOf eas:scheme="NWO-PROJECTNR">my-nwo-related-identifier</dct:isFormatOf>
          <dct:isFormatOf eas:scheme="ISBN">my-isbn-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf eas:scheme="ISSN">my-issn-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf eas:scheme="NWO-PROJECTNR">my-nwo-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf>my own alternative identifier</dct:isFormatOf>
          <eas:relation>
              <eas:subject-title xml:lang="eng">Google</eas:subject-title>
              <eas:subject-link>https://www.google.com</eas:subject-link>
          </eas:relation>
          <eas:replaces>
              <eas:subject-title>urn:nbn:nl:ui:test-urn-related-identifier</eas:subject-title>
              <eas:subject-link>http://persistent-identifier.nl/urn:nbn:nl:ui:test-urn-related-identifier</eas:subject-link>
          </eas:replaces>
          <eas:references>
              <eas:subject-title>10.17026/test-doi-related-identifier</eas:subject-title>
              <eas:subject-link>https://doi.org/10.17026/test-doi-related-identifier</eas:subject-link>
          </eas:references>
          <eas:isFormatOf>
              <eas:subject-title>10.17026/test-doi-alternative-identifier</eas:subject-title>
              <eas:subject-link>https://doi.org/10.17026/test-doi-alternative-identifier</eas:subject-link>
          </eas:isFormatOf>
          <eas:isFormatOf>
              <eas:subject-title>urn:nbn:nl:ui:test-urn-alternative-identifier</eas:subject-title>
              <eas:subject-link>http://persistent-identifier.nl/urn:nbn:nl:ui:test-urn-alternative-identifier</eas:subject-link>
          </eas:isFormatOf>
        </emd:relation>
    ))
    DDM(emd, Seq.empty).map(trim) shouldBe Success(trim( // TODO implemented quick and dirty
      <ddm:DDM xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        <ddm:profile>
          <ddm:accessRights/>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:isFormatOf xsi:type="id-type:NWO-PROJECTNR">my-nwo-related-identifier</dct:isFormatOf>
          <dct:isFormatOf xsi:type="id-type:ISBN">my-isbn-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf xsi:type="id-type:ISSN">my-issn-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf xsi:type="id-type:NWO-PROJECTNR">my-nwo-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf>my own alternative identifier</dct:isFormatOf>
          <dct:hasVersion xsi:type="id-type:ISSN">my-issn-related-identifier</dct:hasVersion>
          <dct:isPartOf>my own related identifier</dct:isPartOf>
          <dct:requires xsi:type="id-type:ISBN">my-isbn-related-identifier</dct:requires>
          <ddm:relation href="https://www.google.com" xml:lang="eng">Google</ddm:relation>
          <ddm:isFormatOf scheme="id-type:DOI" href="https://doi.org/10.17026/test-doi-alternative-identifier">10.17026/test-doi-alternative-identifier</ddm:isFormatOf>
          <ddm:isFormatOf scheme="id-type:URN" href="http://persistent-identifier.nl/urn:nbn:nl:ui:test-urn-alternative-identifier">
            urn:nbn:nl:ui:test-urn-alternative-identifier
          </ddm:isFormatOf>
          <ddm:references scheme="id-type:DOI" href="https://doi.org/10.17026/test-doi-related-identifier">10.17026/test-doi-related-identifier</ddm:references>
          <ddm:replaces scheme="id-type:URN" href="http://persistent-identifier.nl/urn:nbn:nl:ui:test-urn-related-identifier">
            urn:nbn:nl:ui:test-urn-related-identifier
          </ddm:replaces>
          <dct:license xsi:type="dct:URI">{ DDM.dansLicense }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
  }

  "license" should "be copied from <dct:license>" in {
    val emd = parseEmdContent(Seq(
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dct.accessrights">ACCESS_ELSEWHERE</dct:accessRights>
            <dct:license>http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf</dct:license>
            <dct:license eas:scheme="Easy2 version 1">accept</dct:license>
        </emd:rights>
    ))
    // TODO namespace attributes in random order get in the way of trimmed comparison as above
    DDM(emd, Seq.empty).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights>ACCESS_ELSEWHERE</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dct:license xsi:type="dct:URI">${ DDM.dansLicense }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "convert from OPEN_ACCESS" in { // as in streaming.xml
    val emd = parseEmdContent(Seq(
      <emd:rights>
          <dct:accessRights eas:schemeId="common.dct.accessrights">OPEN_ACCESS</dct:accessRights>
      </emd:rights>
    ))
    DDM(emd, Seq.empty).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dct:license xsi:type="dct:URI">${ DDM.cc0 }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "convert from REQUEST_PERMISSION" in { // as in TalkOfEurope.xml
    val emd = parseEmdContent(Seq(
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dct.accessrights">REQUEST_PERMISSION</dct:accessRights>
            <dct:license>accept</dct:license>
        </emd:rights>
    ))
    DDM(emd, Seq.empty).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights>REQUEST_PERMISSION</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dct:license xsi:type="dct:URI">${ DDM.dansLicense }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  "spatial" should "render invalid DDM" in { // TODO until everything is implemented
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial>
              <eas:place/>
              <eas:point eas:scheme="RD">
                  <eas:x>155000</eas:x>
                  <eas:y>463000</eas:y>
              </eas:point>
          </eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    validate(DDM(emd, Seq("D13200")).map(toS)) should matchPattern {
      case Failure(e) if e.getMessage.contains("not:implemented") =>
    }
  }

  "subject" should "succeed" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator,
      <emd:subject>
          <dc:subject eas:scheme="ABR" eas:schemeId="archaeology.dc.subject">DEPO</dc:subject>
          <dc:subject>hello world</dc:subject>
      </emd:subject>,
      emdDescription, emdDates, emdRights,
    ))
    DDM(emd, Seq("D35400")).map(toS).map(strip) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <dc:title>XXX</dc:title>
         |    <dct:description>YYY</dct:description>
         |$ddmCreator
         |    <ddm:created>2017-09-30</ddm:created>
         |    <ddm:available>2017-09-30</ddm:available>
         |    <ddm:audience>D35400</ddm:audience>
         |    <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dc:subject xsi:type="abr:ABRcomplex">DEPO</dc:subject>
         |    <dc:subject>hello world</dc:subject>
         |    <dct:license xsi:type="dct:URI">${ DDM.cc0 }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "generate not-implemented" in {
    val emd = parseEmdContent(Seq(
        <emd:subject>
            <dc:subject eas:scheme="BSS0" eas:schemeId="common.dc.type0" xml:lang="nld-NLD">subject 0</dc:subject>
            <dc:subject eas:scheme="BSS1" eas:schemeId="common.dc.type1" xml:lang="nld-NLD">subject 1</dc:subject>
            <dc:subject xml:lang="nld-NLD" eas:scheme="BSS0">subject zero</dc:subject>
        </emd:subject>
    ))
    DDM(emd, Seq.empty).map(toS).map(strip) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <not:implemented/>
         |    <not:implemented/>
         |    <not:implemented/>
         |    <dct:license xsi:type="dct:URI">${ DDM.dansLicense }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  "author" should "succeed" in {
    val emd = parseEmdContent(Seq(
        <emd:creator>
          <eas:creator>
            <eas:title>Drs</eas:title>
            <eas:initials>P</eas:initials>
            <eas:prefix>van der</eas:prefix>
            <eas:surname>Poel</eas:surname>
            <eas:entityId eas:scheme="DAI">068519397</eas:entityId>
          </eas:creator>
          <eas:creator>
              <eas:initials>X.I.</eas:initials>
              <eas:surname>lastname</eas:surname>
              <eas:entityId eas:identification-system="info:eu-repo/dai/nl/" eas:scheme="DAI">9876543216</eas:entityId>
          </eas:creator>
          <eas:creator>
              <eas:initials>X.I.</eas:initials>
              <eas:surname>lastname</eas:surname>
              <eas:entityId eas:identification-system="http://isni.org/isni/" eas:scheme="ISNI">000000012281955X</eas:entityId>
          </eas:creator>
          <eas:creator>
              <eas:initials>X.I.</eas:initials>
              <eas:surname>lastname</eas:surname>
              <eas:entityId eas:identification-system="https://orcid.org/" eas:scheme="ORCID">0000-0001-2281-955X</eas:entityId>
          </eas:creator>
        </emd:creator>
    ))
    DDM(emd, Seq.empty).map(toS).map(strip) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <dcx-dai:creatorDetails>
         |      <dcx-dai:author>
         |        <dcx-dai:titles>Drs</dcx-dai:titles>
         |        <dcx-dai:initials>P</dcx-dai:initials>
         |        <dcx-dai:insertions>van der</dcx-dai:insertions>
         |        <dcx-dai:surname>Poel</dcx-dai:surname>
         |        <dcx-dai:DAI>info:eu-repo/dai/nl/068519397</dcx-dai:DAI>
         |      </dcx-dai:author>
         |    </dcx-dai:creatorDetails>
         |    <dcx-dai:creatorDetails>
         |      <dcx-dai:author>
         |        <dcx-dai:initials>X.I.</dcx-dai:initials>
         |        <dcx-dai:surname>lastname</dcx-dai:surname>
         |        <dcx-dai:DAI>info:eu-repo/dai/nl/9876543216</dcx-dai:DAI>
         |      </dcx-dai:author>
         |    </dcx-dai:creatorDetails>
         |    <dcx-dai:creatorDetails>
         |      <dcx-dai:author>
         |        <dcx-dai:initials>X.I.</dcx-dai:initials>
         |        <dcx-dai:surname>lastname</dcx-dai:surname>
         |        <dcx-dai:ISNI>http://isni.org/isni/000000012281955X</dcx-dai:ISNI>
         |      </dcx-dai:author>
         |    </dcx-dai:creatorDetails>
         |    <dcx-dai:creatorDetails>
         |      <dcx-dai:author>
         |        <dcx-dai:initials>X.I.</dcx-dai:initials>
         |        <dcx-dai:surname>lastname</dcx-dai:surname>
         |        <dcx-dai:ORCID>https://orcid.org/0000-0001-2281-955X</dcx-dai:ORCID>
         |      </dcx-dai:author>
         |    </dcx-dai:creatorDetails>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dct:license xsi:type="dct:URI">${ DDM.dansLicense }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  "dates" should "use created for available" in {
    val emd = parseEmdContent(Seq(
          <emd:date>
              <dct:created>03-2013</dct:created>
          </emd:date>
    ))
    DDM(emd, Seq.empty).map(toS).map(strip) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:created>03-2013</ddm:created>
         |    <ddm:available>03-2013</ddm:available>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dct:license xsi:type="dct:URI">${ DDM.dansLicense }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "render only the first available" in {
    val emd = parseEmdContent(Seq(
          <emd:date>
              <dc:date>gisteren</dc:date>
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
              <eas:created eas:scheme="W3CDTF" eas:format="DAY">2017-09-30T00:00:00.000+02:00</eas:created>
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
    ))
    DDM(emd, Seq.empty).map(toS).map(strip) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:created>03-2013</ddm:created>
         |    <ddm:created>2017-09-30</ddm:created>
         |    <ddm:created>1901-04</ddm:created>
         |    <ddm:available>04-2013</ddm:available>
         |    <ddm:available>1900</ddm:available>
         |    <ddm:available>1902-04</ddm:available>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dct:date>gisteren</dct:date>
         |    <dct:date>11-2013</dct:date>
         |    <dct:date>12-2013</dct:date>
         |    <dct:date xsi:type="dct:W3CDTF">1909-04</dct:date>
         |    <dct:date xsi:type="dct:W3CDTF">1910-04</dct:date>
         |    <dct:dateCopyrighted>09-2013</dct:dateCopyrighted>
         |    <dct:dateCopyrighted xsi:type="dct:W3CDTF">1907-04</dct:dateCopyrighted>
         |    <dct:dateSubmitted>10-2013</dct:dateSubmitted>
         |    <dct:dateSubmitted xsi:type="dct:W3CDTF">1908-04</dct:dateSubmitted>
         |    <dct:modified>08-2013</dct:modified>
         |    <dct:modified xsi:type="dct:W3CDTF">1906-04</dct:modified>
         |    <dct:issued>07-2013</dct:issued>
         |    <dct:issued xsi:type="dct:W3CDTF">1905-04</dct:issued>
         |    <dct:dateAccepted>05-2013</dct:dateAccepted>
         |    <dct:dateAccepted xsi:type="dct:W3CDTF">1903-04</dct:dateAccepted>
         |    <dct:valid>06-2013</dct:valid>
         |    <dct:valid xsi:type="dct:W3CDTF">1904-04</dct:valid>
         |    <dct:license xsi:type="dct:URI">${ DDM.dansLicense }</dct:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  private def toS(elem: Node) = printer.format(Utility.trim(elem))

  private def toStripped(elem: Elem) = strip(toS(elem))

  private def strip(str: String) = str
    .replaceAll(nameSpaceRegExp, "")
    .replaceAll(" \n", "\n")

  private def validate(triedString: Try[String]): Try[Unit] = {
    assume(schemaIsAvailable)
    triedString.flatMap(validate)
  }

  private def validate(serialized: String): Try[Unit] = {
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

  /** @return a stripped XML compatible with expectedDDM */
  private def normalize(xml: String): String = xml.replaceAll(nameSpaceRegExp, "").replaceAll(" +\n?", " ")

  private def expectedDDM(file: String) = {
    (File("src/test/resources/expected-ddm/") / file)
      .contentAsString.replaceAll(" +", " ")
  }

  private def getEmd(file: DatasetId) = {
    for {
      emdNode <- FoXml.getEmd(XML.loadFile((sampleFoXML / file).toJava))
      emd <- toEmdObject(emdNode)
    } yield emd
  }

  private def toEmdObject(emdNode: Node) = {
    Try(emdUnMarshaller.unmarshal(emdNode.serialize))
  }
}
