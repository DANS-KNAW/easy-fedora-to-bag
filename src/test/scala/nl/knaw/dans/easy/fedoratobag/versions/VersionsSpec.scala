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
package nl.knaw.dans.easy.fedoratobag.versions

import java.io.InputStream

import better.files._
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture
import nl.knaw.dans.easy.fedoratobag.{ FedoraProvider, XmlExtensions }
import org.scalamock.handlers.{ CallHandler1, CallHandler2 }
import org.scalamock.scalatest.MockFactory
import resource.{ DefaultManagedResource, ManagedResource }

import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

class VersionsSpec extends TestSupportFixture with MockFactory {
  class VersionsWithMocks extends Versions {
    override val resolver: Resolver = mock[Resolver]
    override val fedoraProvider: FedoraProvider = mock[FedoraProvider]

    def fedoraExpects(datasetID: String, returning: Elem): CallHandler2[String, String, ManagedResource[InputStream]] = {
      (fedoraProvider.datastream(_: String, _: String)) expects(datasetID, "EMD") once() returning
        new DefaultManagedResource[InputStream](returning.serialize.inputStream)
    }

    def resolverExpects(id: String, returning: Try[String]): CallHandler1[String, Try[String]] =
      (resolver.getDatasetId(_: String)) expects id once() returning returning
  }
  "findChains" should "not loop forever" in {
    val versions = new VersionsWithMocks {
      resolverExpects("easy-dataset:1", returning = Success("easy-dataset:1")) // for readVersionIfo
      resolverExpects("easy-dataset:1", returning = Success("easy-dataset:1")) // for follow
      fedoraExpects("easy-dataset:1",
        returning = <emd:easymetadata>
                      <emd:relation><dct:hasVersion>easy-dataset:1</dct:hasVersion></emd:relation>
                    </emd:easymetadata>
      )
    }
    versions.findChains(Iterator("easy-dataset:1")) shouldBe
      Success(Seq(Seq("easy-dataset:1")))
  }
  it should "not swallow unsafeGetOrThrow in follow" in {
    val versions = new VersionsWithMocks {
      resolverExpects("easy-dataset:1", returning = Success("easy-dataset:1")) // for readVersionIfo
      resolverExpects("easy-dataset:1", returning = Failure(new Exception)) // for follow
      fedoraExpects("easy-dataset:1",
        returning = <emd:easymetadata>
                      <emd:relation><dct:hasVersion>easy-dataset:1</dct:hasVersion></emd:relation>
                    </emd:easymetadata>
      )
    }
    versions.findChains(Iterator("easy-dataset:1")) shouldBe a[Failure[_]]
  }
  it should "follow in both directions" in {
    val emds = Seq(
      "easy-dataset:1" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2019-02-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:hasVersion>easy-dataset:2</dct:hasVersion></emd:relation>
          <emd:relation><dct:replaces>http://www.persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-2ajw-cq</dct:replaces></emd:relation>
        </emd:easymetadata>,
      "easy-dataset:2" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2019-12-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:replacedBy>https://doi.org/10.17026/dans-zjf-522e</dct:replacedBy></emd:relation>
          <emd:relation><dct:isVersionOf>easy-dataset:3</dct:isVersionOf></emd:relation>
        </emd:easymetadata>,
      "easy-dataset:3" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2018-03-23</eas:dateSubmitted></emd:date>
        </emd:easymetadata>,
      "easy-dataset:4" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:date><eas:dateSubmitted>2018-02-12</eas:dateSubmitted></emd:date>
        </emd:easymetadata>,
      "easy-dataset:5" ->
        <emd:easymetadata/>,
      "easy-dataset:6" ->
        <emd:easymetadata xmlns:eas={ VersionInfo.easNameSpace }>
          <emd:relation><dct:isVersionOf>easy-dataset:3</dct:isVersionOf></emd:relation>
        </emd:easymetadata>,
    )
    val versions = new VersionsWithMocks {
      Seq(
        "easy-dataset:1" -> "easy-dataset:1",
        "easy-dataset:2" -> "easy-dataset:2",
        "easy-dataset:3" -> "easy-dataset:3",
        "easy-dataset:5" -> "easy-dataset:5",
        "easy-dataset:6" -> "easy-dataset:6",
        "urn:nbn:nl:ui:13-2ajw-cq" -> "easy-dataset:3",
        "10.17026/dans-zjf-522e" -> "easy-dataset:4",
      ).foreach { case (id, datasetId) => resolverExpects(id, Success(datasetId)) }
      emds.foreach { case (id, emd) => fedoraExpects(id, returning = emd) }
    }

    /* logs:
     *
     * INFO  Family: easy-dataset:2 -> 1577055600000, easy-dataset:1 -> 1550876400000, easy-dataset:4 -> 1518390000000, easy-dataset:3 -> 1521759600000
     * WARN  Family: easy-dataset:5 -> -2208989972000
     * WARN  Family: easy-dataset:6 -> -2208989972000 [1]  Connections: easy-dataset:3
     *
     * warnings in case of default dates (< 1970), see date calculation in VersionInfo.apply
     * nn in "[nn] Connections" implies the number of set operations in Versions.findVersions.connect
     */
    versions.findChains(Iterator("easy-dataset:1", "easy-dataset:5", "easy-dataset:6")) shouldBe
      Success(Seq(
        Seq("easy-dataset:5"),
        Seq("easy-dataset:6", "easy-dataset:4", "easy-dataset:3", "easy-dataset:1", "easy-dataset:2"),
      ))
  }
}
