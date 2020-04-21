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

import java.io.FileInputStream

import better.files.File
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.easy.fedora2vault.Command.FeedBackMessage
import nl.knaw.dans.easy.fedora2vault.fixture.{ FileSystemSupport, TestSupportFixture }
import org.scalamock.scalatest.MockFactory
import resource.managed

import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

class AppSpec extends TestSupportFixture with MockFactory with FileSystemSupport {

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  private val nameSpaceRegExp = """ xmlns:[a-z]+="[^"]*"""" // these attributes have a variable order
  private val samples = File("src/test/resources/sample-foxml")

  private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

  private class MockedApp() extends EasyFedora2vaultApp(null) {
    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
  }

  private class OverriddenApp extends MockedApp {
    /** overrides the method called by the method under test */
    override def simpleTransform(datasetId: DatasetId, outputDir: File): Try[FeedBackMessage] = {
      if (!datasetId.startsWith("success"))
        Failure(new Exception(datasetId))
      else {
        outputDir.createFile()
        Success(s"created $outputDir from $datasetId")
      }
    }
  }

  "simpleTransforms" should "report success" in {
    val input = (testDir / "input").write(
      """success:1
        |success:2
        |""".stripMargin
    )
    val outputDir = (testDir / "output").createDirectories()
    new OverriddenApp().simpleTransForms(input, outputDir) shouldBe
      Success(s"All datasets in $input saved as bags in $outputDir")
    outputDir.list.toSeq.map(_.name) shouldBe Seq("success-1", "success-2")
  }

  it should "report failure" in {
    val input = (testDir / "input").write(
      """success:1
        |failure:1
        |success:2
        |""".stripMargin
    )
    val outputDir = (testDir / "output").createDirectories()
    new OverriddenApp().simpleTransForms(input, outputDir) should matchPattern {
      case Failure(t) if t.getMessage == "failure:1" =>
    }
    outputDir.list.toSeq.map(_.name) shouldBe Seq("success-1")
    // success-2 is not created because of a fail fast strategy
  }

  "simpleTransform" should "produce a bag with EMD" in {
    val emd = <emd:easymetadata xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/" xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/" xmlns:dct="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" emd:version="0.1">
                  <emd:title>
                      <dc:title>Incomplete metadata</dc:title>
                  </emd:title>
              </emd:easymetadata>
    (testDir / "fo.xml").write(createFoXml(emd, "easyadmin").serialize)

    val app = new MockedApp()
    expectAUser(app.ldapContext)
    expectedSubordinates(app.fedoraProvider, "easy-file:35")
    expectedFoXmls(app.fedoraProvider,
        testDir / "fo.xml",
        samples / "easy-file-35.xml",
    )
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "EASY_FILE").write("lalala"),
    )

    app.simpleTransform("easy-dataset:17", testDir / "bag") shouldBe Success("???")

    (testDir / "bag" / "bag-info.txt").contentAsString should startWith("EASY-User-Account: easyadmin")
    (testDir / "bag" / "metadata" / "emd.xml").contentAsString.replaceAll(nameSpaceRegExp, "") shouldBe
      emd.serialize.replaceAll(nameSpaceRegExp, "")
    (testDir / "bag" / "data" / "original" / "P1130783.JPG").contentAsString shouldBe "lalala"
  }

  it should "process DepositApi" in {
    val app = new MockedApp()
    expectedSubordinates(app.fedoraProvider)
    expectedFoXmls(app.fedoraProvider, samples / "DepositApi.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "additional-license").write("lalala"),
      (testDir / "dataset-license").write("blablabla"),
      (testDir / "manifest-sha1.txt").write("rabarbera"),
    )

    app.simpleTransform("easy-dataset:17", testDir / "bag") shouldBe Success("???")

    (testDir / "bag" / "metadata" / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (testDir / "bag" / "metadata" / "license.pdf").contentAsString shouldBe "lalala"
    (testDir / "bag" / "metadata").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf")
    (testDir / "bag" / "metadata" / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf", "message-from-depositor.txt")
  }

  it should "process TalkOfEurope" in {
    val app = new MockedApp()
    expectAUser(app.ldapContext)
    expectedFoXmls(app.fedoraProvider, samples / "TalkOfEurope.xml")
    expectedSubordinates(app.fedoraProvider)
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "dataset-license").write("rabarbera"),
    )

    app.simpleTransform("easy-dataset:12", testDir / "bag") shouldBe Success("???")

    (testDir / "bag" / "metadata" / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "rabarbera"
    (testDir / "bag" / "metadata").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "depositor-info", "emd.xml")
    (testDir / "bag" / "metadata" / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf")
  }

  it should "process streaming" in {
    val app = new MockedApp()
    expectAUser(app.ldapContext)
    expectedFoXmls(app.fedoraProvider, samples / "streaming.xml")
    expectedSubordinates(app.fedoraProvider)

    app.simpleTransform("easy-dataset:13", testDir / "bag") shouldBe Success("???")

    (testDir / "bag" / "metadata").list.toSeq.map(_.name)
      .sortBy(identity) shouldBe Seq("amd.xml", "depositor-info", "emd.xml")
    (testDir / "bag" / "metadata" / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml")
  }

  private def expectedSubordinates(fedoraProvider: => FedoraProvider, expectedIds: String*): Unit = {
    (fedoraProvider.getSubordinates(_: String)) expects * once() returning Success(expectedIds)
  }

  private def expectedManagedStreams(fedoraProvider: => FedoraProvider, expectedObjects: File*): Unit = {
    expectedObjects.foreach(file =>
      (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(*, *) once() returning
        managed(new FileInputStream(file.toJava))
    )
  }

  private def expectedFoXmls(fedoraProvider: => FedoraProvider, expectedObjects: File*): Unit = {
    expectedObjects.foreach(file =>
      (fedoraProvider.getObject(_: String)) expects * once() returning
        managed(new FileInputStream(file.toJava))
    )
  }

  private def expectAUser(ldapContext: => InitialLdapContext) = {
    val result = mock[NamingEnumeration[SearchResult]]
    result.hasMoreElements _ expects() returning true
    val attributes = new BasicAttributes {
      put("displayname", "U.Ser")
      put("mail", "does.not.exist@dans.knaw.nl")
    }
    result.nextElement _ expects() returning new SearchResult("", null, attributes)
    (ldapContext.search(_: String, _: String, _: SearchControls)) expects(*, *, *) returning result
  }

  private def createFoXml(emd: Elem, owner: DatasetId) = {
    // reduced variant of http://deasy.dans.knaw.nl:8080/fedora/objects/easy-dataset:1/objectXML
    <foxml:digitalObject VERSION="1.1" PID="easy-dataset:1"
                   xmlns:foxml="info:fedora/fedora-system:def/foxml#"
                   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                   xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd">
        <foxml:objectProperties>
            <foxml:property NAME="info:fedora/fedora-system:def/model#state" VALUE="Inactive"/>
            <foxml:property NAME="info:fedora/fedora-system:def/model#label" VALUE="DDM example 2 deposited with sword-v1 and UNSPECIFIED as format"/>
            <foxml:property NAME="info:fedora/fedora-system:def/model#ownerId" VALUE={ owner }/>
            <foxml:property NAME="info:fedora/fedora-system:def/model#createdDate" VALUE="2016-11-22T13:11:20.341Z"/>
            <foxml:property NAME="info:fedora/fedora-system:def/view#lastModifiedDate" VALUE="2020-03-17T06:13:44.896Z"/>
        </foxml:objectProperties>
        <foxml:datastream ID="EMD" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
            <foxml:datastreamVersion ID="EMD.1" LABEL="Descriptive metadata for this dataset" CREATED="2016-11-22T13:11:22.765Z" MIMETYPE="text/xml" FORMAT_URI="http://easy.dans.knaw.nl/easy/easymetadata/" SIZE="8119">
                <foxml:contentDigest TYPE="SHA-1" DIGEST="b7f5d6b48483f1f9038e220baae4ec24f768b19a"/>
                <foxml:xmlContent>
                    { emd }
                </foxml:xmlContent>
            </foxml:datastreamVersion>
        </foxml:datastream>
        <foxml:datastream ID="AMD" STATE="A" CONTROL_GROUP="X" VERSIONABLE="false">
            <foxml:datastreamVersion ID="AMD.0" LABEL="Administrative metadata for this dataset" CREATED="2020-03-17T10:24:17.268Z" MIMETYPE="text/xml" SIZE="4807">
                <foxml:xmlContent>
                    <damd:administrative-md xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" version="0.1">
                        <datasetState>SUBMITTED</datasetState>
                        <previousState>DRAFT</previousState>
                    </damd:administrative-md>
                </foxml:xmlContent>
            </foxml:datastreamVersion>
        </foxml:datastream>
    </foxml:digitalObject>
  }
}
