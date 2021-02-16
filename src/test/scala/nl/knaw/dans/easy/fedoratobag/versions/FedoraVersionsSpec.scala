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

import better.files._
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture
import nl.knaw.dans.easy.fedoratobag.{ FedoraProvider, XmlExtensions }
import org.scalamock.handlers.{ CallHandler1, CallHandler2 }
import org.scalamock.scalatest.MockFactory
import resource.{ DefaultManagedResource, ManagedResource }

import java.io.InputStream
import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

class FedoraVersionsSpec extends TestSupportFixture with MockFactory {
  class FedoraVersionsWithMocks extends FedoraVersions(mock[FedoraProvider]) {
    override val resolver: Resolver = mock[Resolver]

    def fedoraExpects(datasetID: String, returning: Elem): CallHandler2[String, String, ManagedResource[InputStream]] = {
      (fedoraProvider.datastream(_: String, _: String)) expects(datasetID, "EMD") once() returning
        new DefaultManagedResource[InputStream](returning.serialize.inputStream)
    }

    def resolverExpects(id: String, returning: Try[String]): CallHandler1[String, Try[String]] =
      (resolver.getDatasetId(_: String)) expects id once() returning returning
  }
  "findChains" should "not loop forever" in {
    val versions = new FedoraVersionsWithMocks {
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
    val versions = new FedoraVersionsWithMocks {
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
        <emd:easymetadata xmlns:eas={ EmdVersionInfo.easNameSpace }>
          <emd:identifier><dc:identifier eas:scheme="DMO_ID">easy-dataset:1</dc:identifier></emd:identifier>
          <emd:date><eas:dateSubmitted>2019-02-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:hasVersion>easy-dataset:2</dct:hasVersion></emd:relation>
          <emd:relation><dct:replaces>http://www.persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-2ajw-cq</dct:replaces></emd:relation>
        </emd:easymetadata>,
      "easy-dataset:2" ->
        <emd:easymetadata xmlns:eas={ EmdVersionInfo.easNameSpace }>
          <emd:identifier><dc:identifier eas:scheme="DMO_ID">easy-dataset:2</dc:identifier></emd:identifier>
          <emd:date><eas:dateSubmitted>2019-12-23</eas:dateSubmitted></emd:date>
          <emd:relation><dct:replacedBy>https://doi.org/10.17026/dans-zjf-522e</dct:replacedBy></emd:relation>
          <emd:relation><dct:isVersionOf>easy-dataset:3</dct:isVersionOf></emd:relation>
        </emd:easymetadata>,
      "easy-dataset:3" ->
        <emd:easymetadata xmlns:eas={ EmdVersionInfo.easNameSpace }>
          <emd:identifier>
            <dc:identifier eas:scheme="DMO_ID">easy-dataset:3</dc:identifier>
            <dc:identifier eas:scheme="PID" eas:identification-system="http://www.persistent-identifier.nl">urn:nbn:nl:ui:13-2ajw-cq</dc:identifier>
          </emd:identifier>
          <emd:date><eas:dateSubmitted>2018-03-23</eas:dateSubmitted></emd:date>
        </emd:easymetadata>,
      "easy-dataset:4" ->
        <emd:easymetadata xmlns:eas={ EmdVersionInfo.easNameSpace }>
          <emd:identifier>
            <dc:identifier eas:scheme="DMO_ID">easy-dataset:4</dc:identifier>
            <dc:identifier eas:scheme="DOI" eas:identification-system="http://dx.doi.org">10.17026/dans-zjf-522e</dc:identifier>          <emd:date><eas:dateSubmitted>2018-02-12</eas:dateSubmitted></emd:date>
          </emd:identifier>
          <emd:date><eas:dateSubmitted>2018-02-12</eas:dateSubmitted></emd:date>
        </emd:easymetadata>,
      "easy-dataset:5" ->
        <emd:easymetadata/>,
      "easy-dataset:6" ->
        <emd:easymetadata xmlns:eas={ EmdVersionInfo.easNameSpace }>
          <emd:identifier><dc:identifier eas:scheme="DMO_ID">easy-dataset:5</dc:identifier></emd:identifier>
          <emd:relation><dct:isVersionOf>easy-dataset:3</dct:isVersionOf></emd:relation>
        </emd:easymetadata>,
    )
    val versions = new FedoraVersionsWithMocks {
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
     * INFO  easy-dataset:1 EmdVersionInfo: 2019-02-23T00:00:00.000+01:00; self=(easy-dataset:1); previous=(urn:nbn:nl:ui:13-2ajw-cq); next=(easy-dataset:2)
     * INFO  easy-dataset:1 following EmdVersionInfo: 2018-03-23T00:00:00.000+01:00; self=(easy-dataset:3,urn:nbn:nl:ui:13-2ajw-cq); previous=(); next=()
     * INFO  easy-dataset:1 following EmdVersionInfo: 2019-12-23T00:00:00.000+01:00; self=(easy-dataset:2); previous=(easy-dataset:3); next=(10.17026/dans-zjf-522e)
     * INFO  easy-dataset:1 following EmdVersionInfo: 2018-02-12T00:00:00.000+01:00; self=(easy-dataset:4,10.17026/dans-zjf-522e); previous=(); next=()
     * INFO  easy-dataset:1 Family[4]: easy-dataset:2 -> 1577055600000, easy-dataset:1 -> 1550876400000, easy-dataset:4 -> 1518390000000, easy-dataset:3 -> 1521759600000 Connections[0]:
     * INFO  easy-dataset:1 new family Map(easy-dataset:2 -> 1577055600000, easy-dataset:1 -> 1550876400000, easy-dataset:4 -> 1518390000000, easy-dataset:3 -> 1521759600000)
     * INFO  easy-dataset:5 EmdVersionInfo: 1900-01-01T00:00:00.000+00:19:32; self=(); previous=(); next=()
     * WARN  easy-dataset:5 Family[1]: easy-dataset:5 -> -2208989972000 Connections[0]:
     * INFO  easy-dataset:5 new family Map(easy-dataset:5 -> -2208989972000)
     * INFO  easy-dataset:6 EmdVersionInfo: 1900-01-01T00:00:00.000+00:19:32; self=(easy-dataset:5); previous=(easy-dataset:3); next=()
     * WARN  easy-dataset:6 Family[1]: easy-dataset:6 -> -2208989972000 Connections[1]: easy-dataset:3
     * INFO  easy-dataset:6 family merged with Map(easy-dataset:2 -> 1577055600000, easy-dataset:1 -> 1550876400000, easy-dataset:4 -> 1518390000000, easy-dataset:3 -> 1521759600000)
     * INFO  easy-dataset:6 new family Map(easy-dataset:2 -> 1577055600000, easy-dataset:1 -> 1550876400000, easy-dataset:4 -> 1518390000000, easy-dataset:3 -> 1521759600000, easy-dataset:6 -> -2208989972000)
     *
     * warnings in case of default dates (< 1970), see date calculation in EmdVersionInfo.apply
     * nn in "[nn] Connections" implies the number of set operations in Versions.findVersions.connect
     */
    versions.findChains(Iterator("easy-dataset:1", "easy-dataset:5", "easy-dataset:6")) shouldBe
      Success(Seq(
        Seq("easy-dataset:5"),
        Seq("easy-dataset:6", "easy-dataset:4", "easy-dataset:3", "easy-dataset:1", "easy-dataset:2"),
      ))
  }
}
