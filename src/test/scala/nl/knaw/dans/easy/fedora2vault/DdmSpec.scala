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
import nl.knaw.dans.easy.fedora2vault.fixture.{ AudienceSupport, TestSupportFixture }

import scala.util.{ Failure, Success, Try }
import scala.xml._

class DdmSpec extends TestSupportFixture with AudienceSupport {

  private val emdNS = "http://easy.dans.knaw.nl/easy/easymetadata/"
  private val easNS = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"
  private val dctNS = "http://purl.org/dc/terms/"
  private val dcNS = "http://purl.org/dc/elements/1.1/"
  private lazy val triedSchema = Try(SchemaFactory
    .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    .newSchema(Array(new StreamSource("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd")).toArray[Source])
  )

  "sample-emd" should "produce the DDMs" in {
    // from easy-dtap/provisioning/roles/easy-test-datasets/files/sdoSets/
    val file = "archaeology.xml"

    val sampleEmd = File("src/test/resources/sample-emd") // TODO more samples
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectedAudiences(Map(
      "easy-discipline:2" -> "D37000",
    ))
    val triedString = Try(XML.loadFile((sampleEmd / file).toJava))
      .flatMap(DDM(_).map(toS))
    triedString.map(normalize) shouldBe Success(expectedDDM(file))
    // TODO
    //     <dc:identifier eas:schemeId="archaeology.dc.identifier" eas:scheme="Archis_onderzoek_m_nr" eas:identification-system="http://archis2.archis.nl">123</dc:identifier>
    //     <dc:identifier eas:scheme="eDNA-project">123</dc:identifier>
    //   currently becomes invalid:
    //     <dcterms:identifier xsi:type="id-type:Archis_onderzoek_m_nr"> 123 </dcterms:identifier>
    //     <dcterms:identifier xsi:type="id-type:eDNA-project">123</dcterms:identifier>
    //   validate(triedString) shouldBe a[Success[_]]
  }

  "TalkOfEurope" should "get a valid DDM out of its EMD" in {
    // easy-dtap/provisioning/roles/easy-test-datasets/files/sdoSets/collectionClarin/easy_dataset_62227/fo.xml
    val file = "TalkOfEurope.xml"

    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectedAudiences(Map(
      "easy-discipline:6" -> "D35400",
      "easy-discipline:11" -> "D34300",
      "easy-discipline:14" -> "D36000",
      "easy-discipline:42" -> "D60000",
    ))
    val triedString = FoXml.getEmd(XML.loadFile((sampleFoXML / file).toJava))
      .flatMap(DDM(_))
    triedString.map(toS).map(normalize) shouldBe Success(expectedDDM(file))
    validate(triedString.map(toS)) shouldBe a[Success[_]]
  }

  "streaming" should "get a valid DDM out of its EMD" in {
    val file = "streaming.xml"

    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectedAudiences(Map("easy-discipline:6" -> "D35400"))
    val triedString = FoXml.getEmd(XML.loadFile((sampleFoXML / file).toJava))
      .flatMap(DDM(_).map(toS))
    triedString.map(normalize) shouldBe Success(expectedDDM(file))
    validate(triedString) shouldBe a[Success[_]]
  }

  "depositApi" should "produce the DDM provided by easy-deposit-api" in {
    val triedFoXml = Try(XML.loadFile((sampleFoXML / "depositApi.xml").toJava))

    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectedAudiences(Map("easy-discipline:77" -> "D13200"))

    val triedDdm = triedFoXml.flatMap(FoXml.getEmd).flatMap(DDM(_).map(toS))
    triedDdm shouldBe a[Success[_]]

    // round trip test (foXml/EMD was created from the foXML/DDM by easy-ingest-flow)
    triedDdm.map(normalize) shouldBe triedFoXml.map(foXml =>
      normalize(toS((foXml \\ "DDM").head))
        .replaceAll("""<dcx-dai:name xml:lang="nld">""", """<dcx-dai:name>""") // TODO api bug? lang on title?
        .split("\n")
        .filterNot(_.contains("<dcterms:rightsHolder>")) // TODO not yet implemented
        .mkString("\n") + "\n"
    )
    validate(triedDdm) shouldBe a[Success[_]]
  }

  "descriptions" should "all appear" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        <emd:description>
          <dc:description>abstract</dc:description>
          <dc:description>Suggestions for data usage: remark1</dc:description>
          <dc:description>beschrijving</dc:description>
          <dct:tableOfContents>rabar</dct:tableOfContents>
          <dct:abstract>blabl</dct:abstract>
        </emd:description>
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <dcterms:description>abstract</dcterms:description>
         |    <dcterms:description>Suggestions for data usage: remark1</dcterms:description>
         |    <dcterms:description>beschrijving</dcterms:description>
         |    <dcterms:description descriptionType="Abstract">blabl</dcterms:description>
         |    <dcterms:description descriptionType="TableOfContent">rabar</dcterms:description>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.cc0 }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  "relations" should "all appear" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        <emd:relation>
        <dc:relation eas:scheme="STREAMING_SURROGATE_RELATION">/domain/dans/user/utest/collection/ctest/presentation/private_continuous</dc:relation>
          <eas:relation eas:emphasis="true">
              <eas:subject-title>zonder qualifier</eas:subject-title>
              <eas:subject-link>https://github.com/DANS-KNAW/easy-dtap/pull/47</eas:subject-link>
          </eas:relation>
          <eas:references eas:emphasis="true">
              <eas:subject-title>met qualifier</eas:subject-title>
              <eas:subject-link>https://github.com/DANS-KNAW/easy-dtap/pull/48</eas:subject-link>
          </eas:references>
        </emd:relation>
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success( // TODO implemented quick and dirty
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <ddm:relation scheme="STREAMING_SURROGATE_RELATION">/domain/dans/user/utest/collection/ctest/presentation/private_continuous</ddm:relation>
         |    <ddm:relation href="https://github.com/DANS-KNAW/easy-dtap/pull/47">zonder qualifier</ddm:relation>
         |    <ddm:references href="https://github.com/DANS-KNAW/easy-dtap/pull/48">met qualifier</ddm:references>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.cc0 }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  "license" should "be copied from <dct:license>" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dcterms.accessrights">ACCESS_ELSEWHERE</dct:accessRights>
            <dct:license>http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf</dct:license>
            <dct:license eas:scheme="Easy2 version 1">accept</dct:license>
        </emd:rights>
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights>ACCESS_ELSEWHERE</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.cc0 }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "convert from OPEN_ACCESS" in { // as in streaming.xml
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dcterms.accessrights">OPEN_ACCESS</dct:accessRights>
        </emd:rights>
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.cc0 }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "convert from REQUEST_PERMISSION" in { // as in TalkOfEurope.xml
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
        <emd:rights>
            <dct:accessRights eas:schemeId="common.dcterms.accessrights">REQUEST_PERMISSION</dct:accessRights>
            <dct:license>accept</dct:license>
        </emd:rights>
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:accessRights>REQUEST_PERMISSION</ddm:accessRights>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.dansLicense }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  "dates" should "use created for available" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    DDM(
      <emd:easymetadata xmlns:emd={ emdNS } xmlns:eas={ easNS } xmlns:dct={ dctNS } xmlns:dc={ dcNS } emd:version="0.1">
          <emd:date>
              <dct:created>03-2013</dct:created>
          </emd:date>
      </emd:easymetadata>
    ).map(toStripped) shouldBe Success(
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:created>03-2013</ddm:created>
         |    <ddm:available>03-2013</ddm:available>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.cc0 }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  it should "render only the first available" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
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
      s"""<ddm:DDM
         |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
         |  <ddm:profile>
         |    <ddm:created>03-2013</ddm:created>
         |    <ddm:created>1901-04</ddm:created>
         |    <ddm:available>04-2013</ddm:available>
         |    <ddm:available>1900</ddm:available>
         |    <ddm:available>1902-04</ddm:available>
         |    <ddm:accessRights/>
         |  </ddm:profile>
         |  <ddm:dcmiMetadata>
         |    <dcterms:date>11-2013</dcterms:date>
         |    <dcterms:date>12-2013</dcterms:date>
         |    <dcterms:date xsi:type="dcterms:W3CDTF">1909-04</dcterms:date>
         |    <dcterms:date xsi:type="dcterms:W3CDTF">1910-04</dcterms:date>
         |    <dcterms:dateCopyrighted>09-2013</dcterms:dateCopyrighted>
         |    <dcterms:dateCopyrighted xsi:type="dcterms:W3CDTF">1907-04</dcterms:dateCopyrighted>
         |    <dcterms:dateSubmitted>10-2013</dcterms:dateSubmitted>
         |    <dcterms:dateSubmitted xsi:type="dcterms:W3CDTF">1908-04</dcterms:dateSubmitted>
         |    <dcterms:modified>08-2013</dcterms:modified>
         |    <dcterms:modified xsi:type="dcterms:W3CDTF">1906-04</dcterms:modified>
         |    <dcterms:issued>07-2013</dcterms:issued>
         |    <dcterms:issued xsi:type="dcterms:W3CDTF">1905-04</dcterms:issued>
         |    <dcterms:dateAccepted>05-2013</dcterms:dateAccepted>
         |    <dcterms:dateAccepted xsi:type="dcterms:W3CDTF">1903-04</dcterms:dateAccepted>
         |    <dcterms:valid>06-2013</dcterms:valid>
         |    <dcterms:valid xsi:type="dcterms:W3CDTF">1904-04</dcterms:valid>
         |    <dcterms:license xsi:type="dcterms:URI">${ DDM.cc0 }</dcterms:license>
         |  </ddm:dcmiMetadata>
         |</ddm:DDM>
         |""".stripMargin)
  }

  private def toStripped(elem: Elem) = toS(elem)
    .replaceAll(nameSpaceRegExp, "")
    .replaceAll(" \n", "\n")

  private def toS(elem: Node) = printer.format(Utility.trim(elem))

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
}
