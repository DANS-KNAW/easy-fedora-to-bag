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

import com.typesafe.scalalogging.Logger
import nl.knaw.dans.easy.fedora2vault.fixture.{ LocalSchemaSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory
import org.slf4j.{ Logger => UnderlyingLogger }

import scala.util.{ Failure, Success }
import scala.xml.{ Node, NodeBuffer }
import scala.xml.Utility.trim

class FileItemSpec extends TestSupportFixture with MockFactory with LocalSchemaSupport {
  val schema = "bag/metadata/files/files.xsd"

  "apply" should "copy both types of rights" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <size>30</size>
                       <creatorRole>DEPOSITOR</creatorRole>
                       <visibleTo>ANONYMOUS</visibleTo>
                       <accessibleTo>RESTRICTED_REQUEST</accessibleTo>

    val triedFileItem = FileItem(fileFoXml(fileMetadata)).map(trim)
    triedFileItem shouldBe Success(trim(
      <file filepath="data/original/something.txt">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>something.txt</dct:title>
        <dct:format>text/plain</dct:format>
        <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
        <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  private def validateItem(item: Node) = validate(FileItem.filesXml(Seq(item)))

  it should "use a default for accessibleTo" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <visibleTo>NONE</visibleTo>

    FileItem(fileFoXml(fileMetadata)).map(trim) shouldBe Success(trim(
      <file filepath="data/original/something.txt">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>something.txt</dct:title>
        <dct:format>text/plain</dct:format>
        <accessibleToRights>NONE</accessibleToRights>
        <visibleToRights>NONE</visibleToRights>
      </file>
    ))
  }

  it should "report a missing tag (this time no default for accessibleTo)" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <visibleTo>ANONYMOUS</visibleTo>

    FileItem(fileFoXml(fileMetadata)) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        "<accessibleTo> not found" =>
    }
  }

  it should "report a repeated tag" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <name>blabla</name>
                       <visibleTo>NONE</visibleTo>

    FileItem(fileFoXml(fileMetadata)) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        "Multiple times <name>" =>
    }
  }


  it should "do additional metadata" in {
    val fileMetadata =
      <name>SKKJ6_spoor.mif</name>
      <path>GIS/SKKJ6_spoor.mif</path>
      <mimeType>application/x-framemaker</mimeType>
      <size>911988</size>
      <creatorRole>ARCHIVIST</creatorRole>
      <visibleTo>ANONYMOUS</visibleTo>
      <accessibleTo>KNOWN</accessibleTo>
      <addmd:additional-metadata>
        <properties></properties>
        <addmd:additional id="addi" label="archaeology-filemetadata">
          <content>
            <file_category>GIS</file_category>
            <original_file>Skkj6_spoor.TAB</original_file>
            <file_content>Alle sporenkwaart</file_content>
            <file_name>SKKJ6_spoor.mif</file_name>
            <file_required>SKKJ6_spoor.mid</file_required>
            <software>MapInfo</software>
            <analytic_units>antropogene en natuurlijke sporen</analytic_units>
            <mapprojection>non-earth (in m.), met de waarden van het RD-stelsel</mapprojection>
            <notes>alle sporen samen vormen de putomtrek</notes>
          </content>
        </addmd:additional>
      </addmd:additional-metadata>

    FileItem(fileFoXml(fileMetadata)).map(trim) shouldBe Success(trim(
      <file filepath="data/GIS/SKKJ6_spoor.mif">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>SKKJ6_spoor.mif</dct:title>
        <dct:format>application/x-framemaker</dct:format>
        <accessibleToRights>KNOWN</accessibleToRights>
        <visibleToRights>ANONYMOUS</visibleToRights>
        <dct:file_category>GIS</dct:file_category>
        <dct:isFormatOf>Skkj6_spoor.TAB</dct:isFormatOf>
        <dct:abstract>Alle sporenkwaart</dct:abstract>
        <dct:title>SKKJ6_spoor.mif</dct:title>
        <dct:requires>SKKJ6_spoor.mid</dct:requires>
        <dct:software>MapInfo</dct:software>
        <dct:analytic_units>antropogene en natuurlijke sporen</dct:analytic_units>
        <dct:mapprojection>non-earth (in m.), met de waarden van het RD-stelsel</dct:mapprojection>
        <dct:notes>alle sporen samen vormen de putomtrek</dct:notes>
      </file>
    ))
  }

  "checkNotImplemented" should "report items once" in {
    val items =
      <file filepath="data/GIS/SKKJ6_spoor.mif">
        <dct:identifier>easy-file:35</dct:identifier>
        <notImplemented>analytic_units: abc</notImplemented>
        <notImplemented>mapprojection: xyz</notImplemented>
      </file>
      <file filepath="rabarbera.txt">
        <dct:identifier>easy-file:78</dct:identifier>
      </file>
      <file filepath="blabla.txt">
        <dct:identifier>easy-file:78</dct:identifier>
        <notImplemented>analytic_units: blabla</notImplemented>
        <notImplemented>opmerkingen: rabarbera</notImplemented>
      </file>

    val mockLogger = mock[UnderlyingLogger]
    Seq(
      "easy-file:35 (data/GIS/SKKJ6_spoor.mif) NOT IMPLEMENTED: analytic_units: abc",
      "easy-file:35 (data/GIS/SKKJ6_spoor.mif) NOT IMPLEMENTED: mapprojection: xyz",
      "easy-file:78 (blabla.txt) NOT IMPLEMENTED: analytic_units: blabla",
      "easy-file:78 (blabla.txt) NOT IMPLEMENTED: opmerkingen: rabarbera",
    ).foreach(s => (mockLogger.warn(_: String)) expects s once())
    (() => mockLogger.isWarnEnabled()) expects() anyNumberOfTimes() returning true

    FileItem.checkNotImplemented(items.toList, Logger(mockLogger)) should matchPattern {
      case Failure(e) if e.getMessage == "2 file(s) with not implemented additional file metadata: List(analytic_units, mapprojection, opmerkingen)" =>
    }
  }

  private def fileFoXml(fileMetadata: NodeBuffer) = {
      <foxml:digitalObject VERSION="1.1" PID="easy-file:35"
             xmlns:foxml="info:fedora/fedora-system:def/foxml#"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd">
          <foxml:datastream ID="EASY_FILE_METADATA" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
              <foxml:datastreamVersion ID="EASY_FILE_METADATA.0" LABEL="" CREATED="2020-03-17T10:24:17.660Z" MIMETYPE="text/xml" SIZE="359">
                  <foxml:xmlContent>
                      <fimd:file-item-md xmlns:addmd="http://easy.dans.knaw.nl/easy/additional-metadata/" xmlns:fimd="http://easy.dans.knaw.nl/easy/file-item-md/" version="0.1">
                          { fileMetadata }
                      </fimd:file-item-md>
                  </foxml:xmlContent>
              </foxml:datastreamVersion>
          </foxml:datastream>
      </foxml:digitalObject>
  }
}
