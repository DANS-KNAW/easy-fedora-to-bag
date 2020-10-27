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
package nl.knaw.dans.easy.fedoratobag

import better.files.File
import nl.knaw.dans.easy.fedoratobag.fixture.{ AudienceSupport, EmdSupport, SchemaSupport, TestSupportFixture }
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller

import scala.util.{ Failure, Success, Try }
import scala.xml._

class DdmSpec extends TestSupportFixture with EmdSupport with AudienceSupport with SchemaSupport {
  System.setProperty("http.agent", "Test")

  override val schema = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

  private val schemaLocation = "http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
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

  /** expected results of the emd values above */

  private val ddmCreator =
          <dcx-dai:creatorDetails>
            <dcx-dai:organization>
              <dcx-dai:name>DANS</dcx-dai:name>
            </dcx-dai:organization>
          </dcx-dai:creatorDetails>

  private def ddmProfile(audience: String, creator: Elem = ddmCreator) =
       <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { creator }
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>{ audience }</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>

  "streaming" should "get a valid DDM out of its EMD" in {
    val file = "streaming.xml"
    val triedDdm = getEmd(file).flatMap(DDM(_, Seq("D35400"), abrMapping))
    // Logs
    //  INFO  skipped dct:relation STREAMING_SURROGATE_RELATION /domain/dans/user/utest/collection/ctest/presentation/private_continuous
    val expectedDdm = (File("src/test/resources/expected-ddm/") / file)
      .contentAsString
      .replaceAll(" +", " ")
      .replaceAll("\n +<", "\n<").trim
      .replace("\n<dct:relation/>", "").trim
    triedDdm.map(normalized) shouldBe Success(expectedDdm)
    assume(schemaIsAvailable)
    triedDdm.flatMap(validate) shouldBe Success(())
  }

  "depositApi" should "produce the DDM provided by easy-deposit-api" in {
    val triedFoXml = Try(XML.loadFile((sampleFoXML / "DepositApi.xml").toJava))
    val triedDdm = getEmd("DepositApi.xml").flatMap(DDM(_, Seq("D13200"), abrMapping))
    triedDdm shouldBe a[Success[_]]

    // round trip test: create DDM from EMD
    // * easy-deposit-api created DDM + EMD
    // * easy-ingest-flow copied both into foXml and extended EMD, not DDM
    triedDdm.map(normalized(_).split("\n").filterNot(_.contains("URN")).filterNot(_.contains("EASY2")).mkString("\n")
    ) shouldBe triedFoXml.map(foXml =>
      normalized((foXml \\ "DDM").head)
        // just another prefix for the namespace
        .replaceAll("dcterms:", "dct:")
        // TODO api bug? lang on title?
        .replaceAll("""<dcx-dai:name xml:lang="nld">""", """<dcx-dai:name>""")
    )
    assume(schemaIsAvailable)
    triedDdm.flatMap(validate) shouldBe Success(())
  }

  "descriptions" should "all appear" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator,
        <emd:description>
          <dc:description>abstract</dc:description>
          <dc:description>Suggestions for data usage: remark1</dc:description>
          <dc:description>beschrijving</dc:description>
          <dct:tableOfContents>rabar</dct:tableOfContents>
          <dct:abstract>blabl</dct:abstract>
        </emd:description>,
      emdDates,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>abstract</dct:description>
          <dct:description>Suggestions for data usage: remark1</dct:description>
          <dct:description>beschrijving</dct:description>
          { ddmCreator }
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <ddm:description descriptionType="Abstract">blabl</ddm:description>
          <ddm:description descriptionType="TableOfContents">rabar</ddm:description>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
       </ddm:DDM>
     ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "relations" should "all appear except STREAMING_SURROGATE_RELATION" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:relation>
          <dc:relation eas:scheme="STREAMING_SURROGATE_RELATION">/domain/dans/user/utest/collection/ctest/presentation/private_continuous</dc:relation>
          <dct:hasVersion eas:scheme="ISSN">my-issn-related-identifier</dct:hasVersion>
          <dct:requires eas:scheme="ISBN">my-isbn-related-identifier</dct:requires>
          <dct:isPartOf>my own related identifier</dct:isPartOf>
          <dct:isFormatOf eas:scheme="NWO-projectnummer">my-nwo-related-identifier</dct:isFormatOf>
          <dct:isFormatOf eas:scheme="ISBN">my-isbn-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf eas:scheme="ISSN">my-issn-alternative-identifier</dct:isFormatOf>
          <dct:isFormatOf eas:scheme="NWO-projectnummer">my-nwo-alternative-identifier</dct:isFormatOf>
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
        </emd:relation>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    // logs INFO  skipped dct:relation STREAMING_SURROGATE_RELATION /domain/dans/user/utest/collection/ctest/presentation/private_continuous
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
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
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "identifiers" should "produce " in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
      <emd:identifier>
        <dc:identifier
          eas:scheme="Archis_onderzoek_m_nr"
          eas:schemeId="archaeology.dc.identifier"
          eas:identification-system="https://archis.cultureelerfgoed.nl"
      >4763492100</dc:identifier>
      </emd:identifier>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <dct:identifier xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">4763492100</dct:identifier>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "creator" should "not produce an empty role" in {
    val emd = parseEmdContent(Seq(
      emdTitle,
      <emd:creator>
        <eas:creator>
            <eas:title>Drs.</eas:title>
            <eas:initials>E.A.</eas:initials>
            <eas:surname>Schorn</eas:surname>
            <eas:organization>KSP Archeologie vof</eas:organization>
            <eas:entityId eas:scheme="DAI"></eas:entityId>
            <eas:role eas:scheme="DATACITE"></eas:role>
        </eas:creator>
      </emd:creator>,
      emdDescription, emdDates, emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:titles>Drs.</dcx-dai:titles>
              <dcx-dai:initials>E.A.</dcx-dai:initials>
              <dcx-dai:surname>Schorn</dcx-dai:surname>
              <dcx-dai:organization>
                <dcx-dai:name>KSP Archeologie vof</dcx-dai:name>
              </dcx-dai:organization>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>{ "D35400" }</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "license" should "be copied from <dct:license>" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dct.accessrights">ACCESS_ELSEWHERE</dct:accessRights>
            <dct:license>http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf</dct:license>
            <dct:license eas:scheme="Easy2 version 1">accept</dct:license>
        </emd:rights>
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>ACCESS_ELSEWHERE</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.dansLicense }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should matchPattern {
      case Failure(e) if e.getMessage.contains("Value 'ACCESS_ELSEWHERE' is not facet-valid") =>
    }
  }

  it should "convert from OPEN_ACCESS" in { // as in streaming.xml
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
      <emd:rights>
          <dct:accessRights eas:schemeId="common.dct.accessrights">OPEN_ACCESS</dct:accessRights>
      </emd:rights>
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  it should "convert from REQUEST_PERMISSION" in { // as in TalkOfEurope.xml
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dct.accessrights">REQUEST_PERMISSION</dct:accessRights>
            <dct:license>accept</dct:license>
        </emd:rights>
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>REQUEST_PERMISSION</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.dansLicense }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "spatial" should "render a point" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial><eas:point eas:scheme="RD"><eas:x>700</eas:x><eas:y>456000</eas:y></eas:point></eas:spatial>
          <eas:spatial><eas:point eas:scheme="degrees"><eas:x>52.08110</eas:x><eas:y>4.34521</eas:y></eas:point></eas:spatial>
          <eas:spatial><eas:point><eas:x>1</eas:x><eas:y>2</eas:y></eas:point></eas:spatial>
          <eas:spatial><eas:point><eas:x>1</eas:x></eas:point></eas:spatial>
          <eas:spatial><eas:point><eas:y>2</eas:y></eas:point></eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    // logs ERROR not implemented invalid point [SpatialPoint(Some(RD),None,None)]
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
           <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
             <Point xmlns="http://www.opengis.net/gml"><pos>700 456000</pos></Point>
           </dcx-gml:spatial>
           <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
             <Point xmlns="http://www.opengis.net/gml"><pos>4.34521 52.08110</pos></Point>
           </dcx-gml:spatial>
           <dcx-gml:spatial><Point xmlns="http://www.opengis.net/gml"><pos>2 1</pos></Point></dcx-gml:spatial>
           <dcx-gml:spatial><Point xmlns="http://www.opengis.net/gml"><pos>0 1</pos></Point></dcx-gml:spatial>
           <dcx-gml:spatial><Point xmlns="http://www.opengis.net/gml"><pos>2 0</pos></Point></dcx-gml:spatial>
           <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
      )
    ) // note that a missing x or y defaults to zero
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  it should "report a point without coordinates as not implemented" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial><eas:point eas:scheme="RD"></eas:point></eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    // logs ERROR not implemented invalid point [SpatialPoint(Some(RD),None,None)]
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <not:implemented/>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should failWithNotImplementedElement
  }

  it should "render a polygon" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial>
            <eas:polygon eas:scheme="RD">
              <eas:place>Some kind of description, without an actual polygon attached to it</eas:place>
            </eas:polygon>
          </eas:spatial>
          <eas:spatial>
            <eas:polygon eas:scheme="degrees">
              <eas:place>A triangle between DANS, NWO and the railway station</eas:place>
              <eas:polygon-exterior>
                <eas:place>main triangle</eas:place>
                <eas:polygon-point><eas:x>52.08110</eas:x><eas:y>4.34521</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.08071</eas:x><eas:y>4.34422</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.07913</eas:x><eas:y>4.34332</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.08110</eas:x><eas:y>4.34521</eas:y></eas:polygon-point>
              </eas:polygon-exterior>
              <eas:polygon-interior>
                <eas:place>hole1</eas:place>
                <eas:polygon-point><eas:x>52.080542</eas:x><eas:y>4.344215</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.080450</eas:x><eas:y>4.344323</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.080357</eas:x><eas:y>4.344110</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.080542</eas:x><eas:y>4.344215</eas:y></eas:polygon-point>
              </eas:polygon-interior>
              <eas:polygon-interior>
                <eas:place>hole2</eas:place>
                <eas:polygon-point><eas:x>52.080542</eas:x><eas:y>4.344215</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.080450</eas:x><eas:y>4.344323</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.080357</eas:x><eas:y>4.344110</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>52.080542</eas:x><eas:y>4.344215</eas:y></eas:polygon-point>
              </eas:polygon-interior>
            </eas:polygon>
          </eas:spatial>
          <eas:spatial>
            <eas:polygon eas:scheme="RD">
              <eas:place>A triangle between DANS, NWO and the railway station</eas:place>
              <eas:polygon-interior>
                <eas:place>hole in none</eas:place>
                <eas:polygon-point><eas:x>83506</eas:x><eas:y>455210</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>83513</eas:x><eas:y>455200</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>83499</eas:x><eas:y>455189</eas:y></eas:polygon-point>
                <eas:polygon-point><eas:x>83506</eas:x><eas:y>455210</eas:y></eas:polygon-point>
              </eas:polygon-interior>
            </eas:polygon>
          </eas:spatial>
          <eas:spatial>
            <eas:polygon eas:scheme="RD">
              <eas:place>A triangle between DANS, NWO and the railway station</eas:place>
              <eas:polygon-interior>
                <eas:place>pointless hole</eas:place>
              </eas:polygon-interior>
            </eas:polygon>
          </eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized(_).replace("<posList></posList>", "<posList/>")) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
           <dcx-gml:spatial>
             <Polygon srsName="http://www.opengis.net/def/crs/EPSG/0/28992" xmlns="http://www.opengis.net/gml">
               <name>Some kind of description, without an actual polygon attached to it</name>
             </Polygon>
           </dcx-gml:spatial>
           <dcx-gml:spatial>
             <Polygon srsName="http://www.opengis.net/def/crs/EPSG/0/4326" xmlns="http://www.opengis.net/gml">
               <name>A triangle between DANS, NWO and the railway station</name>
               <exterior><LinearRing><description>main triangle</description><posList>4.34521 52.08110 4.34422 52.08071 4.34332 52.07913 4.34521 52.08110</posList></LinearRing></exterior>
               <interior><LinearRing><description>hole1</description><posList>4.344215 52.080542 4.344323 52.080450 4.344110 52.080357 4.344215 52.080542</posList></LinearRing></interior>
               <interior><LinearRing><description>hole2</description><posList>4.344215 52.080542 4.344323 52.080450 4.344110 52.080357 4.344215 52.080542</posList></LinearRing></interior>
             </Polygon>
           </dcx-gml:spatial>
           <dcx-gml:spatial>
             <Polygon srsName="http://www.opengis.net/def/crs/EPSG/0/28992" xmlns="http://www.opengis.net/gml">
               <name>A triangle between DANS, NWO and the railway station</name>
               <interior><LinearRing><description>hole in none</description><posList>83506 455210 83513 455200 83499 455189 83506 455210</posList></LinearRing></interior>
             </Polygon>
           </dcx-gml:spatial>
           <dcx-gml:spatial>
             <Polygon srsName="http://www.opengis.net/def/crs/EPSG/0/28992" xmlns="http://www.opengis.net/gml">
               <name>A triangle between DANS, NWO and the railway station</name>
               <interior><LinearRing><description>pointless hole</description><posList></posList></LinearRing></interior>
             </Polygon>
           </dcx-gml:spatial>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    )) // Note that even te validation is happy with a pointless polygon or an interior without exterior
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  it should "render a box" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial>
            <eas:box eas:scheme="RD">
              <eas:north>455271.2</eas:north>
              <eas:east>83575.4</eas:east>
              <eas:south>455271.0</eas:south>
              <eas:west>83575.0</eas:west>
            </eas:box>
          </eas:spatial>
          <eas:spatial>
            <eas:box eas:scheme="degrees">
              <eas:north>79.5</eas:north>
              <eas:east>23.0</eas:east>
              <eas:south>76.7</eas:south>
              <eas:west>10.0</eas:west>
            </eas:box>
          </eas:spatial>
          <eas:spatial>
            <eas:box eas:scheme="RD">
              <eas:west>83575.0</eas:west>
            </eas:box>
          </eas:spatial>
          <eas:spatial>
            <eas:box eas:scheme="degrees">
                <eas:north>79.5</eas:north>
            </eas:box>
          </eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    // logs
    //  ERROR not implemented invalid box [SpatialBox(Some(RD),None,None,None,None)]
    //  ERROR not implemented invalid box [SpatialBox(Some(degrees),None,None,None,None)]
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
           <dcx-gml:spatial>
             <boundedBy xmlns="http://www.opengis.net/gml">
               <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
                 <lowerCorner>83575.0 455271.0</lowerCorner>
                 <upperCorner>83575.4 455271.2</upperCorner>
               </Envelope>
             </boundedBy>
           </dcx-gml:spatial>
           <dcx-gml:spatial>
             <boundedBy xmlns="http://www.opengis.net/gml">
               <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
                 <lowerCorner>76.7 10.0</lowerCorner>
                 <upperCorner>79.5 23.0</upperCorner>
               </Envelope>
             </boundedBy>
           </dcx-gml:spatial>
           <dcx-gml:spatial>
             <boundedBy xmlns="http://www.opengis.net/gml">
               <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
                 <lowerCorner>83575.0 0</lowerCorner>
                 <upperCorner>0 0</upperCorner>
               </Envelope>
             </boundedBy>
           </dcx-gml:spatial>
           <dcx-gml:spatial>
             <boundedBy xmlns="http://www.opengis.net/gml">
               <Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
                 <lowerCorner>0 0</lowerCorner>
                 <upperCorner>79.5 0</upperCorner>
               </Envelope>
             </boundedBy>
           </dcx-gml:spatial>
           <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  it should "report a box without any coordinate as not implemented" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial>
            <eas:box eas:scheme="RD">
            </eas:box>
          </eas:spatial>
          <eas:spatial>
            <eas:box eas:scheme="degrees">
            </eas:box>
          </eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    // logs
    //  ERROR not implemented expected either point, box or polygon [name=A general description ]
    //  ERROR not implemented expected either point, box or polygon [scheme=null x=1 y=nullscheme=degrees north=79.5 east=23.0 south=76.7 west=10.0]
    //  ERROR not implemented expected either point, box or polygon [scheme=degrees north=79.5 east=23.0 south=76.7 west=10.0(exterior=null, interior=null) ]
    //  ERROR not implemented expected either point, box or polygon [scheme=null x=1 y=null(exterior=null, interior=null) ]
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <not:implemented/>
          <not:implemented/>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should failWithNotImplementedElement
  }

  it should "report a mix of spatial element types" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <eas:spatial>
            <eas:place>A general description</eas:place>
          </eas:spatial>
          <eas:spatial>
            <eas:point><eas:x>1</eas:x></eas:point>
            <eas:box eas:scheme="degrees"><eas:north>79.5</eas:north><eas:east>23.0</eas:east><eas:south>76.7</eas:south><eas:west>10.0</eas:west></eas:box>
          </eas:spatial>
          <eas:spatial>
            <eas:box eas:scheme="degrees"><eas:north>79.5</eas:north><eas:east>23.0</eas:east><eas:south>76.7</eas:south><eas:west>10.0</eas:west></eas:box>
            <eas:polygon eas:scheme="RD"><eas:place>A polygon description</eas:place></eas:polygon>
          </eas:spatial>
          <eas:spatial>
            <eas:point><eas:x>1</eas:x></eas:point>
            <eas:polygon eas:scheme="RD"><eas:place>A polygon description</eas:place></eas:polygon>
          </eas:spatial>
        </emd:coverage>,
      emdRights,
    ))
    // logs
    //  ERROR not implemented  [subject 0]
    //  ERROR not implemented  [subject 1]
    //  ERROR not implemented  [subject z
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <not:implemented/>
          <not:implemented/>
          <not:implemented/>
          <not:implemented/>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should failWithNotImplementedElement
  }

  "subject" should "succeed" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator,
      <emd:subject>
          <dc:subject eas:scheme="ABR" eas:schemeId="archaeology.dc.subject">DEPO</dc:subject>
          <dc:subject>hello world</dc:subject>
          <dc:subject eas:scheme="BSS0" eas:schemeId="common.dc.type0" xml:lang="nld-NLD">subject 0</dc:subject>
          <dc:subject eas:scheme="BSS1" eas:schemeId="common.dc.type1" xml:lang="nld-NLD">subject 1</dc:subject>
          <dc:subject xml:lang="nld-NLD" eas:scheme="BSS0">subject zero</dc:subject>
      </emd:subject>,
      emdDescription, emdDates,
      <emd:format>
          <dc:format eas:scheme="scheme60" xml:lang="en-US" eas:schemeId="schemeId56">format0</dc:format>
          <dc:format eas:scheme="scheme61" xml:lang="en-US" eas:schemeId="schemeId57">format1</dc:format>
          <dct:extent eas:scheme="scheme62" xml:lang="en-US" eas:schemeId="schemeId58">extent0</dct:extent>
          <dct:extent eas:scheme="scheme63" xml:lang="en-US" eas:schemeId="schemeId59">extent1</dct:extent>
          <dct:medium eas:scheme="scheme64" xml:lang="en-US" eas:schemeId="schemeId60">medium0</dct:medium>
          <dct:medium eas:scheme="scheme65" xml:lang="en-US" eas:schemeId="schemeId61">medium1</dct:medium>
      </emd:format>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    // logs (but bag will get validated)
    //  ERROR not implemented  [subject 0]
    //  ERROR not implemented  [subject 1]
    //  ERROR not implemented  [subject zero]
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <dct:format>format0</dct:format>
          <dct:format>format1</dct:format>
          <dct:extent>extent0</dct:extent>
          <dct:extent>extent1</dct:extent>
          <dct:medium>medium0</dct:medium>
          <dct:medium>medium1</dct:medium>
          <ddm:subject  xml:lang="nl"
                        valueURI="http://www.rnaproject.org/data/b97ef902-059a-4c14-b925-273c74bace30"
                        subjectScheme="Archeologisch Basis Register"
                        schemeURI="http://www.rnaproject.org"
          >Depot (DEPO)</ddm:subject>
          <dct:subject>hello world</dct:subject>
          <dct:subject xml:lang="nld-NLD" xsi:type="-">subject 0</dct:subject>
          <dct:subject xml:lang="nld-NLD" xsi:type="-">subject 1</dct:subject>
          <dct:subject xml:lang="nld-NLD" xsi:type="-">subject zero</dct:subject>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should failWithNotImplementedAttribute
  }

  it should "report not implemented attributes" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator,
      <emd:subject>
          <dc:subject eas:scheme="ABR" eas:schemeId="archaeologie.subject">DEPO</dc:subject>
      </emd:subject>,
      emdDescription, emdDates, emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    // logs ERROR not implemented ABR schemeId [DEPO]
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <dct:subject xsi:type="-">DEPO</dct:subject>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should failWithNotImplementedAttribute
  }

  it should "map ABR" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator,
        <emd:subject>
          <dc:subject eas:scheme="ABR" eas:schemeId="archaeology.dc.subject">NX</dc:subject>
        </emd:subject>,
      emdDescription, emdDates,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <ddm:subject xml:lang="nl"
                       valueURI="http://www.rnaproject.org/data/85ae2aa0-caae-4745-aecb-6cc765a8782f"
                       subjectScheme="Archeologisch Basis Register"
                       schemeURI="http://www.rnaproject.org"
          >Nederzetting, onbepaald (NX)</ddm:subject>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "relation" should "not throw exceptions" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
      <emd:relation>
          <eas:isPartOf>
              <eas:subject-title>Second Timothy: When and Where? Text and Traditions in the Subscriptions</eas:subject-title>
          </eas:isPartOf>
          <eas:isPartOf>
              <eas:subject-title>Briefrapport</eas:subject-title>
              <eas:subject-link/>
          </eas:isPartOf>
          <eas:isPartOf>
              <eas:subject-title>2005-09/11</eas:subject-title>
              <eas:subject-link/>
          </eas:isPartOf>
          <eas:isPartOf>
              <eas:subject-title>blabla</eas:subject-title>
          </eas:isPartOf>
          <eas:references>
              <eas:subject-title>Archeologisch onderzoek verbreding Hunzeloop Elzemaat</eas:subject-title>
              <eas:subject-link>urn:nbn:nl:ui:13-svxg-8g</eas:subject-link>
          </eas:references>
          <eas:references>
              <eas:subject-title>Acabadabra</eas:subject-title>
              <eas:subject-link>10.17026/dans-something</eas:subject-link>
          </eas:references>
      </emd:relation>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D13200"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D13200") }
        <ddm:dcmiMetadata>
          <ddm:references href="https://persistent-identifier.nl/urn:nbn:nl:ui:13-svxg-8g">Archeologisch onderzoek verbreding Hunzeloop Elzemaat</ddm:references>
          <ddm:references>Acabadabra</ddm:references>
          <ddm:isPartOf>Second Timothy: When and Where? Text and Traditions in the Subscriptions</ddm:isPartOf>
          <ddm:isPartOf>Briefrapport</ddm:isPartOf>
          <ddm:isPartOf>2005-09/11</ddm:isPartOf>
          <ddm:isPartOf>blabla</ddm:isPartOf>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  it should "report not implemented href" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
      <emd:relation>
          <eas:references>
              <eas:subject-title>rababera</eas:subject-title>
              <eas:subject-link>isbn:9053566937</eas:subject-link>
          </eas:references>
      </emd:relation>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D13200"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D13200") }
        <ddm:dcmiMetadata>
          <not:implemented/>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
  }

  "temporal coverage" should "map ABR" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription, emdDates,
        <emd:coverage>
          <dct:temporal eas:scheme="ABR" eas:schemeId="archaeology.dct.temporal">ROM</dct:temporal>
          <dct:temporal eas:scheme="ABR" eas:schemeId="archaeology.dct.temporal">XME</dct:temporal>
        </emd:coverage>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        { ddmProfile("D35400") }
        <ddm:dcmiMetadata>
          <ddm:temporal xml:lang="en"
                        valueURI="http://www.rnaproject.org/data/000c6eeb-83ac-47d5-b18f-c9e5d5f08b69"
                        subjectScheme="Archeologisch Basis Register"
                        schemeURI="http://www.rnaproject.org"
          >Roman period (ROM)</ddm:temporal>
          <ddm:temporal xml:lang="en"
                        valueURI="http://www.rnaproject.org/data/9deee0d5-bf7f-48ab-9d17-c42f30fdfcce"
                        subjectScheme="Archeologisch Basis Register"
                        schemeURI="http://www.rnaproject.org"
          >Middle Ages (XME)</ddm:temporal>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "author" should "succeed" in {
    val emd = parseEmdContent(Seq(
      emdTitle,
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
        </emd:creator>,
      emdDescription, emdDates, emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:titles>Drs</dcx-dai:titles>
              <dcx-dai:initials>P</dcx-dai:initials>
              <dcx-dai:insertions>van der</dcx-dai:insertions>
              <dcx-dai:surname>Poel</dcx-dai:surname>
              <dcx-dai:DAI>info:eu-repo/dai/nl/068519397</dcx-dai:DAI>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:initials>X.I.</dcx-dai:initials>
              <dcx-dai:surname>lastname</dcx-dai:surname>
              <dcx-dai:DAI>info:eu-repo/dai/nl/9876543216</dcx-dai:DAI>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:initials>X.I.</dcx-dai:initials>
              <dcx-dai:surname>lastname</dcx-dai:surname>
              <dcx-dai:ISNI>http://isni.org/isni/000000012281955X</dcx-dai:ISNI>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <dcx-dai:creatorDetails>
            <dcx-dai:author>
              <dcx-dai:initials>X.I.</dcx-dai:initials>
              <dcx-dai:surname>lastname</dcx-dai:surname>
              <dcx-dai:ORCID>https://orcid.org/0000-0001-2281-955X</dcx-dai:ORCID>
            </dcx-dai:author>
          </dcx-dai:creatorDetails>
          <ddm:created>2017-09-30</ddm:created>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  "dates" should "use created for available" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription,
          <emd:date>
              <dct:created>03-2013</dct:created>
          </emd:date>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>03-2013</ddm:created>
          <ddm:available>03-2013</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should matchPattern {
      case Failure(e) if e.getMessage
        .contains("'03-2013' is not a valid value of union type '#AnonType_W3CDTF'") =>
    }
  }

  it should "render an invalid number of dates created" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription,
          <emd:date>
              <dct:created>2013-03</dct:created>
              <dct:available>2013-04</dct:available>
              <eas:created eas:scheme="W3CDTF" eas:format="DAY">2017-09-30T00:00:00.000+02:00</eas:created>
              <eas:created eas:scheme="W3CDTF" eas:format="MONTH">1901-04-01T00:00:00.000+00:19:32</eas:created>
          </emd:date>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>2013-03</ddm:created>
          <ddm:created>2017-09-30</ddm:created>
          <ddm:created>1901-04</ddm:created>
          <ddm:available>2013-04</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should matchPattern {
      case Failure(e) if e.getMessage
        .contains("Invalid content was found starting with element 'ddm:created'") && e.getMessage
        .contains(":available}' is expected") =>
    }
  }

  it should "render an invalid number of dates available" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription,
          <emd:date>
              <dct:created>2013-03</dct:created>
              <dct:available>2013-04</dct:available>
              <eas:available eas:scheme="W3CDTF" eas:format="DAY">2017-09-30T00:00:00.000+02:00</eas:available>
              <eas:available eas:scheme="W3CDTF" eas:format="MONTH">1901-04-01T00:00:00.000+00:19:32</eas:available>
          </emd:date>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>2013-03</ddm:created>
          <ddm:available>2013-04</ddm:available>
          <ddm:available>2017-09-30</ddm:available>
          <ddm:available>1901-04</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) should matchPattern {
      case Failure(e) if e.getMessage
        .contains("Invalid content was found starting with element 'ddm:available'") && e.getMessage
        .contains(":audience}' is expected") =>
    }
  }

  it should "render dates with proper precision" in {
    val emd = parseEmdContent(Seq(
      emdTitle, emdCreator, emdDescription,
          <emd:date>
              <dc:date>gisteren</dc:date>
              <dc:date>11-2013</dc:date>
              <dc:date>12-2013</dc:date>
              <dct:created>2013-03</dct:created>
              <dct:valid>06-2013</dct:valid>
              <dct:available>2013-04</dct:available>
              <dct:issued>07-2013</dct:issued>
              <dct:modified>08-2013</dct:modified>
              <dct:dateAccepted>05-2013</dct:dateAccepted>
              <dct:dateCopyrighted>09-2013</dct:dateCopyrighted>
              <dct:dateSubmitted>10-2013</dct:dateSubmitted>
              <eas:date eas:scheme="W3CDTF" eas:format="MONTH">1909-04-01T00:00:00.000+00:19:32</eas:date>
              <eas:date eas:scheme="W3CDTF" eas:format="MONTH">1910-04-01T00:00:00.000+00:19:32</eas:date>
              <eas:valid eas:scheme="W3CDTF" eas:format="MONTH">1904-04-01T00:00:00.000+00:19:32</eas:valid>
              <eas:issued eas:scheme="W3CDTF" eas:format="MONTH">1905-04-01T00:00:00.000+00:19:32</eas:issued>
              <eas:modified eas:scheme="W3CDTF" eas:format="MONTH">1906-04-01T00:00:00.000+00:19:32</eas:modified>
              <eas:dateAccepted eas:scheme="W3CDTF" eas:format="MONTH">1903-04-01T00:00:00.000+00:19:32</eas:dateAccepted>
              <eas:dateCopyrighted eas:scheme="W3CDTF" eas:format="MONTH">1907-04-01T00:00:00.000+00:19:32</eas:dateCopyrighted>
              <eas:dateSubmitted eas:scheme="W3CDTF" eas:format="MONTH">1908-04-01T00:00:00.000+00:19:32</eas:dateSubmitted>
          </emd:date>,
      emdRights,
    ))
    val triedDDM = DDM(emd, Seq("D35400"), abrMapping)
    triedDDM.map(normalized) shouldBe Success(normalized(
      <ddm:DDM xsi:schemaLocation={ schemaLocation }>
        <ddm:profile>
          <dc:title>XXX</dc:title>
          <dct:description>YYY</dct:description>
          { ddmCreator }
          <ddm:created>2013-03</ddm:created>
          <ddm:available>2013-04</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dct:date>gisteren</dct:date>
          <dct:date>11-2013</dct:date>
          <dct:date>12-2013</dct:date>
          <dct:date xsi:type="dct:W3CDTF">1909-04</dct:date>
          <dct:date xsi:type="dct:W3CDTF">1910-04</dct:date>
          <dct:dateCopyrighted>09-2013</dct:dateCopyrighted>
          <dct:dateCopyrighted xsi:type="dct:W3CDTF">1907-04</dct:dateCopyrighted>
          <dct:dateSubmitted>10-2013</dct:dateSubmitted>
          <dct:dateSubmitted xsi:type="dct:W3CDTF">1908-04</dct:dateSubmitted>
          <dct:modified>08-2013</dct:modified>
          <dct:modified xsi:type="dct:W3CDTF">1906-04</dct:modified>
          <dct:issued>07-2013</dct:issued>
          <dct:issued xsi:type="dct:W3CDTF">1905-04</dct:issued>
          <dct:dateAccepted>05-2013</dct:dateAccepted>
          <dct:dateAccepted xsi:type="dct:W3CDTF">1903-04</dct:dateAccepted>
          <dct:valid>06-2013</dct:valid>
          <dct:valid xsi:type="dct:W3CDTF">1904-04</dct:valid>
          <dct:license xsi:type="dct:URI">{ DDM.cc0 }</dct:license>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    ))
    assume(schemaIsAvailable)
    triedDDM.flatMap(validate) shouldBe Success(())
  }

  private def normalized(elem: Node) = printer
    .format(Utility.trim(elem)) // this trim normalizes <a/> and <a></a>
    .replaceAll(nameSpaceRegExp, "") // the random order would cause differences in actual and expected
    .replaceAll(" +\n?", " ")
    .replaceAll("\n +<", "\n<")
    .trim

  private def getEmd(file: String) = {
    for {
      emdNode <- FoXml.getEmd(XML.loadFile((sampleFoXML / file).toJava))
      emd <- Try(emdUnMarshaller.unmarshal(emdNode.serialize))
    } yield emd
  }

  private def failWithNotImplementedElement = {
    matchPattern {
      case Failure(e) if e.getMessage
        .contains("""The prefix "not" for element "not:implemented" is not bound.""") =>
    }
  }

  private def failWithNotImplementedAttribute = {
    matchPattern {
      case Failure(e) if e.getMessage
        .contains("'-' is not a valid value for 'QName'.") =>
    }
  }
}
