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
import scala.xml.{ Elem, SAXParseException, Utility, XML }

class DdmSpec extends TestSupportFixture with AudienceSupport {

  private val emdNS = "http://easy.dans.knaw.nl/easy/easymetadata/"
  private val easNS = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"
  private val dctNS = "http://purl.org/dc/terms/"
  private val dcNS = "http://purl.org/dc/elements/1.1/"
  private lazy val triedSchema = Try(SchemaFactory
    .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    .newSchema(Array(new StreamSource("https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd")).toArray[Source])
  )

  "TalkOfEurope" should "get a DDM out of its EMD" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectedAudiences(Map(
      "easy-discipline:6" -> "D35400",
      "easy-discipline:11" -> "D34300",
      "easy-discipline:14" -> "D36000",
      "easy-discipline:42" -> "D60000",
    ))

    val expected = File("src/test/resources/expected-ddm/TalkOfEurope.xml")
      .contentAsString.replaceAll(" +", " ")

    val triedString = FoXml.getEmd(XML.loadFile((samples / "TalkOfEurope.xml").toJava))
      .flatMap(DDM(_).map(toS))
    triedString.map(_.replaceAll(nameSpaceRegExp, "").replaceAll(" +\n?", " ")) shouldBe Success(expected)

    assume(schemaIsAvailable)
    triedString.flatMap(validate) shouldBe a[Success[_]]
  }

  "descriptions" should "..." in {
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
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <dcterms:description>abstract</dcterms:description>
        |    <dcterms:description>Suggestions for data usage: remark1</dcterms:description>
        |    <dcterms:description>beschrijving</dcterms:description>
        |    <dcterms:description descriptionType="Abstract">blabl</dcterms:description>
        |    <dcterms:description descriptionType="TableOfContent">rabar</dcterms:description>
        |    <ddm:accessRights/>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata/>
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
      """<ddm:DDM
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        |  <ddm:profile>
        |    <ddm:created>03-2013</ddm:created>
        |    <ddm:available>03-2013</ddm:available>
        |    <ddm:accessRights/>
        |  </ddm:profile>
        |  <ddm:dcmiMetadata/>
        |</ddm:DDM>
        |""".stripMargin)
  }

  it should "tolerate an empty EMD" in {
    implicit val fedoraProvider: FedoraProvider = mock[FedoraProvider]
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
      """<ddm:DDM
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

  private def toStripped(elem: Elem) = toS(elem)
    .replaceAll(nameSpaceRegExp, "")
    .replaceAll(" \n", "\n")

  private def toS(elem: Elem) = printer.format(Utility.trim(elem))

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
