/*
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

import com.typesafe.scalalogging.Logger
import nl.knaw.dans.easy.fedoratobag.fixture.{ FileFoXmlSupport, SchemaSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory
import org.slf4j.{ Logger => UnderlyingLogger }

import scala.util.{ Failure, Success }
import scala.xml.Utility.trim
import scala.xml._

class FileItemSpec extends TestSupportFixture with MockFactory with SchemaSupport with FileFoXmlSupport {
  val ns = "http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
  val schema: String = ns + "files.xsd"
  val location = s"$ns $schema"

  private def validateItem(item: Node) = validate(FileItem.filesXml(Seq(item)))

  private val derivedFiles = callFileInfo(Map(
    "easy-file:2" -> fileFoXml(id = 2, name = "so:me.xslx", creatorRole = "ARCHIVIST"),
    "easy-file:25" -> fileFoXml(id = 25, location = "cu*rated", name = "so:me.csv", derivedFrom = Some(2)),
  )).getOrElse(fail("could not load test data"))

  "filesXml" should "drop original in <dct:source>" in {
    val fileItems = derivedFiles.map(FileItem(_, isOriginalVersioned = true).get)
    normalized(FileItem.filesXml(fileItems)) shouldBe normalized(
      <files xsi:schemaLocation={ location } xmlns={ ns }>
        <file filepath="data/so_me.xslx">
          <!--original/so_me.xslx-->
          <dct:identifier>easy-file:2</dct:identifier>
          <dct:title>so_me.xslx</dct:title>
          <dct:format>text/plain</dct:format>
          <dct:extent>0.0MB</dct:extent>
          <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
          <afm:keyvaluepair>
            <afm:key>ORIGINAL_FILE_PATH</afm:key>
            <afm:value>so:me.xslx</afm:value>
          </afm:keyvaluepair>
        </file>
        <file filepath="data/cu_rated/so_me.csv"><dct:identifier>easy-file:25</dct:identifier>
          <dct:title>so_me.csv</dct:title>
          <dct:format>text/plain</dct:format>
          <dct:extent>0.0MB</dct:extent>
          <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
          <afm:keyvaluepair>
            <afm:key>ORIGINAL_FILE_PATH</afm:key>
            <afm:value>cu*rated/so:me.csv</afm:value>
          </afm:keyvaluepair>
          <dct:source>data/so_me.xslx</dct:source>
        </file>
      </files>
    )
  }
  it should "keep original in <dct:source>" in {
    val fileItems = derivedFiles.map(FileItem(_, isOriginalVersioned = false).get)
    normalized(FileItem.filesXml(fileItems)) shouldBe normalized(
      <files xsi:schemaLocation={ location } xmlns={ ns }>
        <file filepath="data/original/so_me.xslx">
          <dct:identifier>easy-file:2</dct:identifier>
          <dct:title>so_me.xslx</dct:title>
          <dct:format>text/plain</dct:format>
          <dct:extent>0.0MB</dct:extent>
          <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
          <afm:keyvaluepair>
            <afm:key>ORIGINAL_FILE_PATH</afm:key>
            <afm:value>original/so:me.xslx</afm:value>
          </afm:keyvaluepair>
        </file><file filepath="data/cu_rated/so_me.csv">
          <dct:identifier>easy-file:25</dct:identifier>
          <dct:title>so_me.csv</dct:title>
          <dct:format>text/plain</dct:format>
          <dct:extent>0.0MB</dct:extent>
          <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
          <afm:keyvaluepair>
            <afm:key>ORIGINAL_FILE_PATH</afm:key>
            <afm:value>cu*rated/so:me.csv</afm:value>
          </afm:keyvaluepair>
          <dct:source>data/original/so_me.xslx</dct:source>
        </file>
      </files>
    )
  }

  // TODO share with DdmSpec
  private def normalized(elem: Node) = new PrettyPrinter(160, 2)
    .format(Utility.trim(elem)) // this trim normalizes <a/> and <a></a>
    .replaceAll(nameSpaceRegExp, "") // the random order would cause differences in actual and expected
    .replaceAll(" +\n?", " ")
    .replaceAll("\n +<", "\n<")
    .trim

  "apply" should "copy both types of rights" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <size>30000000</size>
                       <creatorRole>DEPOSITOR</creatorRole>
                       <visibleTo>ANONYMOUS</visibleTo>
                       <accessibleTo>RESTRICTED_REQUEST</accessibleTo>
    val fileInfo = createFileInfo(fileMetadata)

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/original/something.txt">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>something.txt</dct:title>
        <dct:format>text/plain</dct:format>
        <dct:extent>28.6MB</dct:extent>
        <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
        <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  it should "use a default for accessibleTo" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <size>100000</size>
                       <mimeType>text/plain</mimeType>
                       <visibleTo>NONE</visibleTo>
    val fileInfo = createFileInfo(fileMetadata)

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/original/something.txt">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>something.txt</dct:title>
        <dct:format>text/plain</dct:format>
        <dct:extent>0.1MB</dct:extent>
        <accessibleToRights>NONE</accessibleToRights>
        <visibleToRights>NONE</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  it should "report a missing tag (this time no default for accessibleTo)" in {
    val fileMetadata = <name>something.txt</name>
                       <path>original/something.txt</path>
                       <mimeType>text/plain</mimeType>
                       <size>30</size>
                       <visibleTo>ANONYMOUS</visibleTo>
    callFileInfo(Map(
      "easy-file:35" -> fileFoXml(fileMetadata),
    )) should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        "easy-file:35 <accessibleTo> not found" =>
    }
  }

  it should "add location url" in {
    val fileInfo = callFileInfo(Map(
      "easy-file:35" -> XML.loadFile("src/test/resources/sample-foxml/invalidUrl.xml",
      ))).getOrElse(fail).head

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/audio-video/konijn.mp4">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>konijn.mp4</dct:title>
        <dct:format>video/mp4</dct:format>
        <dct:extent>1.0MB</dct:extent>
        <accessibleToRights>NONE</accessibleToRights>
        <visibleToRights>NONE</visibleToRights>
        <dct:source>http://legacy-storage.dans.knaw.nl/data/Getuigenverhalen/Project_Het Vergeten_Bombardement/GV_GAR_bombardement_05.mp4</dct:source>
      </file>
    ))

  }

  it should "map key-value pairs" in {
    val fileMetadata = {
      <sid>easy-file:3165833</sid>
      <name>R0011867.jpg</name>
      <parentSid>easy-folder:169296</parentSid>
      <datasetSid>easy-dataset:50012</datasetSid>
      <path>Fotos/R0011867.jpg</path>
      <mimeType>image/jpeg</mimeType>
      <size>2927389</size>
      <creatorRole>ARCHIVIST</creatorRole>
      <visibleTo>ANONYMOUS</visibleTo>
      <accessibleTo>RESTRICTED_REQUEST</accessibleTo>
      <addmd:additional-metadata>
          <properties></properties>
          <addmd:additional id="addi" label="archaeology-filemetadata">
              <content>
                  <original_file>R0011867.JPG</original_file>
                  <file_name>R0011867.jpg</file_name>
                  <file_content>Veldfoto</file_content>
                  <othmat_codebook>Fotolijst.csv</othmat_codebook>
                  <DIA_KL_ZW>X</DIA_KL_ZW>
                  <DATUM>9-12-2009</DATUM>
                  <WERKPUT>2</WERKPUT>
                  <VLAK>1</VLAK>
                  <PROFIEL>N</PROFIEL>
                  <FOTOGRAAF>RvW</FOTOGRAAF>
                  <RICHTING>N</RICHTING>
                  <OMSCHRIJVING>foto van het ingestorte noordprofiel (met ladder zichtbaar)</OMSCHRIJVING>
                  <ID-Foto>18435</ID-Foto>
              </content>
          </addmd:additional>
      </addmd:additional-metadata>
    }
    val fileInfo = createFileInfo(fileMetadata)

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/Fotos/R0011867.jpg">
          <dct:identifier>easy-file:35</dct:identifier>
          <dct:title>R0011867.jpg</dct:title>
          <dct:format>image/jpeg</dct:format>
          <dct:extent>2.8MB</dct:extent>
          <dct:isFormatOf>R0011867.JPG</dct:isFormatOf>
          <dct:title>R0011867.jpg</dct:title>
          <dct:abstract>Veldfoto</dct:abstract>
          <afm:othmat_codebook>Fotolijst.csv</afm:othmat_codebook>
          <afm:keyvaluepair><afm:key>DIA_KL_ZW</afm:key><afm:value>X</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>DATUM</afm:key><afm:value>9-12-2009</afm:value>
          </afm:keyvaluepair><afm:keyvaluepair><afm:key>WERKPUT</afm:key><afm:value>2</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>VLAK</afm:key><afm:value>1</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>PROFIEL</afm:key><afm:value>N</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>FOTOGRAAF</afm:key><afm:value>RvW</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>RICHTING</afm:key><afm:value>N</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>OMSCHRIJVING</afm:key><afm:value>foto van het ingestorte noordprofiel (met ladder zichtbaar)</afm:value></afm:keyvaluepair>
          <afm:keyvaluepair><afm:key>ID-Foto</afm:key><afm:value>18435</afm:value></afm:keyvaluepair>
          <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  it should "skip empty key-value pairs" in {
    val fileMetadata = {
      <sid>easy-file:3165833</sid>
      <name>R0011867.jpg</name>
      <parentSid>easy-folder:169296</parentSid>
      <datasetSid>easy-dataset:50012</datasetSid>
      <path>Fotos/R0011867.jpg</path>
      <mimeType>image/jpeg</mimeType>
      <size>2927389</size>
      <creatorRole>ARCHIVIST</creatorRole>
      <visibleTo>ANONYMOUS</visibleTo>
      <accessibleTo>RESTRICTED_REQUEST</accessibleTo>
      <addmd:additional-metadata>
          <properties></properties>
          <addmd:additional id="addi" label="archaeology-filemetadata">
              <content>
                  <original_file>R0011867.JPG</original_file>
                  <file_name>R0011867.jpg</file_name>
                  <file_content>Veldfoto</file_content>
                  <othmat_codebook>Fotolijst.csv</othmat_codebook>
                  <DIA_KL_ZW></DIA_KL_ZW>
              </content>
          </addmd:additional>
      </addmd:additional-metadata>
    }
    val fileInfo = createFileInfo(fileMetadata)

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/Fotos/R0011867.jpg">
          <dct:identifier>easy-file:35</dct:identifier>
          <dct:title>R0011867.jpg</dct:title>
          <dct:format>image/jpeg</dct:format>
          <dct:extent>2.8MB</dct:extent>
          <dct:isFormatOf>R0011867.JPG</dct:isFormatOf>
          <dct:title>R0011867.jpg</dct:title>
          <dct:abstract>Veldfoto</dct:abstract>
          <afm:othmat_codebook>Fotolijst.csv</afm:othmat_codebook>
          <accessibleToRights>RESTRICTED_REQUEST</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  it should "use file name as second title (first title from mandatory name)" in {
    val fileMetadata = {
      <name>SKKJ6_spoor.mix</name>
      <path>GIS/SKKJ6_spoor.mix</path>
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
   }
    val fileInfo = createFileInfo(fileMetadata)

    // note that we have two times <dct:title>,
    // once from <name>, once from <addmd:additional-metadata><file_name>
    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/GIS/SKKJ6_spoor.mix">
        <dct:identifier>easy-file:35</dct:identifier>
        <dct:title>SKKJ6_spoor.mix</dct:title>
        <dct:format>application/x-framemaker</dct:format>
        <dct:extent>0.9MB</dct:extent>
        <afm:file_category>GIS</afm:file_category>
        <dct:isFormatOf>Skkj6_spoor.TAB</dct:isFormatOf>
        <dct:abstract>Alle sporenkwaart</dct:abstract>
        <dct:title>SKKJ6_spoor.mif</dct:title>
        <dct:requires>SKKJ6_spoor.mid</dct:requires>
        <afm:software>MapInfo</afm:software>
        <afm:analytic_units>antropogene en natuurlijke sporen</afm:analytic_units>
        <afm:mapprojection>non-earth (in m.), met de waarden van het RD-stelsel</afm:mapprojection>
        <afm:notes>alle sporen samen vormen de putomtrek</afm:notes>
        <accessibleToRights>KNOWN</accessibleToRights>
        <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  it should "use archival_name as title and skip quantity 1" in {
    val fileMetadata = {
      <name>A</name>
      <path>B/A</path>
      <mimeType>C</mimeType>
      <size>9</size>
      <creatorRole>D</creatorRole>
      <visibleTo>ANONYMOUS</visibleTo>
      <accessibleTo>ANONYMOUS</accessibleTo>
      <addmd:additional-metadata>
        <addmd:additional id="addi" label="archaeology-filemetadata">
          <content>
            <file_name>G</file_name>
            <archival_name>I</archival_name>
            <file_required>J</file_required>
            <file_content>K</file_content>
            <notes>L</notes>
            <remarks>M</remarks>
            <file_notes>N</file_notes>
            <file_remarks>O</file_remarks>
            <case_quantity> 1 </case_quantity>
            <case_quantity>22</case_quantity>
          </content>
        </addmd:additional>
      </addmd:additional-metadata>
    }
    val fileInfo = createFileInfo(fileMetadata)

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/B/A">
          <dct:identifier>easy-file:35</dct:identifier>
          <dct:title>A</dct:title>
          <dct:format>C</dct:format>
          <dct:extent>0.0MB</dct:extent>
          <dct:isFormatOf>G</dct:isFormatOf>
          <dct:title>I</dct:title>
          <dct:requires>J</dct:requires>
          <dct:abstract>K</dct:abstract>
          <afm:notes>L</afm:notes>
          <afm:notes>M</afm:notes>
          <afm:notes>N</afm:notes>
          <afm:notes>O</afm:notes>
          <afm:case_quantity>22</afm:case_quantity>
          <accessibleToRights>ANONYMOUS</accessibleToRights>
          <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    ))
    assume(schemaIsAvailable)
    triedFileItem.flatMap(validateItem) shouldBe Success(())
  }

  "checkNotImplemented" should "no longer report an original_file combined with archival_name" in {
    val fileMetadata = {
      <name>A</name>
      <path>B/A</path>
      <mimeType>C</mimeType>
      <size>9</size>
      <creatorRole>D</creatorRole>
      <visibleTo>E</visibleTo>
      <accessibleTo>F</accessibleTo>
      <addmd:additional-metadata>
        <addmd:additional id="addi" label="archaeology-filemetadata">
          <content>
            <blabla>K</blabla>
            <file_name>G</file_name>
            <original_file>I</original_file>
            <archival_name>j</archival_name>
          </content>
        </addmd:additional>
      </addmd:additional-metadata>
    }
    val fileInfo = createFileInfo(fileMetadata)

    val triedFileItem = FileItem(fileInfo, isOriginalVersioned = false)
    triedFileItem.map(trim) shouldBe Success(trim(
      <file filepath="data/B/A">
          <dct:identifier>easy-file:35</dct:identifier>
          <dct:title>A</dct:title>
          <dct:format>C</dct:format>
          <dct:extent>0.0MB</dct:extent>
          <afm:keyvaluepair><afm:key>blabla</afm:key><afm:value>K</afm:value></afm:keyvaluepair>
          <dct:title>G</dct:title>
          <dct:isFormatOf>I</dct:isFormatOf>
          <dct:title>j</dct:title>
          <accessibleToRights>F</accessibleToRights>
          <visibleToRights>E</visibleToRights>
      </file>
    ))
    val mockLogger = mock[UnderlyingLogger]
    FileItem.checkNotImplementedFileMetadata(List(triedFileItem.get), Logger(mockLogger)) shouldBe a[Success[_]]
  }

  it should "report items once" in {
    val items = {
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
    }

    val mockLogger = mock[UnderlyingLogger]
    Seq(
      "easy-file:35 (data/GIS/SKKJ6_spoor.mif) NOT IMPLEMENTED: analytic_units: abc",
      "easy-file:35 (data/GIS/SKKJ6_spoor.mif) NOT IMPLEMENTED: mapprojection: xyz",
      "easy-file:78 (blabla.txt) NOT IMPLEMENTED: analytic_units: blabla",
      "easy-file:78 (blabla.txt) NOT IMPLEMENTED: opmerkingen: rabarbera",
    ).foreach(s => (mockLogger.warn(_: String)) expects s once())
    (() => mockLogger.isWarnEnabled()) expects() anyNumberOfTimes() returning true

    FileItem.checkNotImplementedFileMetadata(items.toList, Logger(mockLogger)) should matchPattern {
      case Failure(e) if e.getMessage == "2 file(s) with not implemented additional file metadata: List(analytic_units, mapprojection, opmerkingen)" =>
    }
  }

  private def createFileInfo(fileMetadata: NodeBuffer) = {
    callFileInfo(Map(
      "easy-file:35" -> fileFoXml(fileMetadata),
    )).map(_.head).getOrElse(fail("could not load test data"))
  }

  private def callFileInfo(foXMLs: Map[Depositor, Elem]) = {
    val fedoraProvider = mock[FedoraProvider]
    foXMLs.foreach { case (id, xml) =>
      (fedoraProvider.loadFoXml(_: String)) expects id once() returning Success(xml)
    }
    val triedFileInfos = FileInfo(foXMLs.keys.toList, fedoraProvider, false)
    triedFileInfos
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
