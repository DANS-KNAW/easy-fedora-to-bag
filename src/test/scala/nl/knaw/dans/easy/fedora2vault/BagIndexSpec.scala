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

import java.net.URI

import nl.knaw.dans.easy.fedora2vault.fixture.TestSupportFixture
import org.scalamock.scalatest.MockFactory
import scalaj.http.HttpResponse

import scala.util.{ Failure, Success }
import scala.xml.SAXParseException

class BagIndexSpec extends TestSupportFixture with MockFactory {

  "bagInfoByDoi" should "return None" in {
    new BagIndex(new URI("https://does.not.exist.dans.knaw.nl")) {
      override def execute(doi: String): HttpResponse[String] =
        new HttpResponse[String](body = "", code = 400, headers = Map.empty)
    }.bagInfoByDoi("") shouldBe Success(None)
  }

  it should "return also None" in {
    new BagIndex(new URI("https://does.not.exist.dans.knaw.nl")) {
      override def execute(doi: String): HttpResponse[String] =
        new HttpResponse[String](body = "<result/>", code = 200, headers = Map.empty)
    }.bagInfoByDoi("") shouldBe Success(None)
  }

  it should "return Some" in {
    new BagIndex(new URI("https://does.not.exist.dans.knaw.nl")) {
      override def execute(doi: String): HttpResponse[String] =
        new HttpResponse[String](body = "<result><bag-info>blabla</bag-info></result>", code = 200, headers = Map.empty)
    }.bagInfoByDoi("") shouldBe Success(Some("<bag-info>blabla</bag-info>"))
  }

  it should "return SAXParseException" in {
    new BagIndex(new URI("https://does.not.exist.dans.knaw.nl")) {
      override def execute(doi: String): HttpResponse[String] =
      // TODO apply as bagIndexExpects in SimpleCheckerSpec
        new HttpResponse[String](body = "", code = 200, headers = Map.empty)
    }.bagInfoByDoi("") should matchPattern {
      case Failure(e: SAXParseException) if e.getMessage == "Premature end of file." =>
    }
  }

  it should "return not expected response code" in {
    new BagIndex(new URI("https://does.not.exist.dans.knaw.nl")) {
      override def execute(doi: String): HttpResponse[String] =
        new HttpResponse[String](body = "", code = 300, headers = Map.empty)
    }.bagInfoByDoi("") should matchPattern {
      case Failure(e: Exception) if e.getMessage ==
        "Not expected response code from bag-index. url='https://does.not.exist.dans.knaw.nlsearch', doi='', response: 300 - " =>
    }
  }
}
