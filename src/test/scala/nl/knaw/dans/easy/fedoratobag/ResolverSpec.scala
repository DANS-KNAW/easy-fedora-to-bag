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

import java.net.{ SocketTimeoutException, UnknownHostException }

import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture
import nl.knaw.dans.easy.fedoratobag.versions.Resolver

import scala.util.{ Failure, Success }

class ResolverSpec extends TestSupportFixture {

  "getDatasetId" should "return input" in {
    Resolver.getDatasetId("easy-dataset:123") shouldBe
      Success("easy-dataset:123")
  }
  it should "find doi" in {
    Resolver.getDatasetId("10.17026/dans-zjf-522e") match {
      case Success(s) => s shouldBe "easy-dataset:34340"
      case Failure(e) => assume(serviceAvailable(e))
    }
  }

  it should "find urn" in {
    Resolver.getDatasetId("urn:nbn:nl:ui:13-2ajw-cq") match {
      case Success(s) => s shouldBe "easy-dataset:46789"
      case Failure(e) => assume(serviceAvailable(e))
    }
  }
  it should "not find garbage doi" in {
    val doi = "10.17026/does-not-exist"
    Resolver.getDatasetId(doi) match {
      case Success(_) => fail("not expecting success")
      case Failure(e) => assume(serviceAvailable(e))
        e.getMessage should startWith(
          s"Not expected response code from 'https://doi.org/$doi' 404"
        )
    }
  }
  it should "not find garbage urn" in {
    val urn = "urn:nbn:nl:ui:13-does-not-exist"
    Resolver.getDatasetId(urn) match {
      case Success(_) => fail("not expecting success")
      case Failure(e) => assume(serviceAvailable(e))
        e.getMessage should startWith(
          s"Not expected response code from 'http://www.persistent-identifier.nl/?identifier=$urn' 200"
        )
    }
  }

  private def serviceAvailable(e: Throwable) = {
    !e.isInstanceOf[UnknownHostException] &&
      !e.isInstanceOf[SocketTimeoutException]
  }
}
