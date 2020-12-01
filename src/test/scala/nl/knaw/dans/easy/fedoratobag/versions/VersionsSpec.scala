package nl.knaw.dans.easy.fedoratobag.versions

import java.io.InputStream

import better.files._
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture
import nl.knaw.dans.easy.fedoratobag.{ FedoraProvider, XmlExtensions }
import org.scalamock.scalatest.MockFactory
import resource.DefaultManagedResource

import scala.util.Success

class VersionsSpec extends TestSupportFixture with MockFactory {
  "self reference" should "not loop forever" in {
    val provider = mock[FedoraProvider]

    Seq(
      "easy-dataset:1" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2018-02-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:hasVersion>easy-dataset:1</dct:hasVersion></emd:relation>
        </emd:easymetadata>
    ).foreach { case (datasetID, emd) =>
      (provider.datastream(_: String, _: String)) expects(datasetID, "EMD") once() returning
        new DefaultManagedResource[InputStream](emd.serialize.inputStream)
    }
    Versions.findVersions("easy-dataset:1", provider) shouldBe
      Success(Seq("easy-dataset:1"))
  }
  "findVersions" should "follow via easy-datasetid" in {
    val provider = mock[FedoraProvider]

    Seq(
      "easy-dataset:4" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2018-02-12</eas:dateSubmitted></emd:date>
        </emd:easymetadata>,
      "easy-dataset:3" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2018-03-23</eas:dateSubmitted></emd:date>
        </emd:easymetadata>,
      "easy-dataset:1" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2019-02-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:hasVersion>easy-dataset:2</dct:hasVersion></emd:relation>
          <emd:relation><dct:isVersionOf>easy-dataset:3</dct:isVersionOf></emd:relation>
        </emd:easymetadata>,
      "easy-dataset:2" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2019-12-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:hasVersion>easy-dataset:4</dct:hasVersion></emd:relation>
          <emd:relation><dct:isVersionOf>easy-dataset:3</dct:isVersionOf></emd:relation>
        </emd:easymetadata>,
    ).foreach { case (datasetID, emd) =>
      (provider.datastream(_: String, _: String)) expects(datasetID, "EMD") once() returning
        new DefaultManagedResource[InputStream](emd.serialize.inputStream)
    }
    Versions.findVersions("easy-dataset:1", provider) shouldBe
      Success(Seq("easy-dataset:4", "easy-dataset:3", "easy-dataset:1", "easy-dataset:2"))
  }
}
