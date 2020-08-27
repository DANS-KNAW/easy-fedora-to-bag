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

import java.io.IOException
import java.net.URI

import better.files.StringExtensions
import scalaj.http.{ Http, HttpResponse }

import scala.util.{ Failure, Try }
import scala.xml.XML

case class BagIndex(bagIndexUri: URI) {

  /** An IOException is fatal for the batch of datasets */
  private case class BagIndexException(msg: String, cause: Throwable) extends IOException(msg, cause)

  private val url: URI = bagIndexUri.resolve("/search")

  def bagInfoByDoi(doi: String): Try[Option[String]] = for {
    maybeString <- findBagInfo(doi)
    maybeXml = maybeString.map(s => XML.load(s.inputStream))
    maybeBagInfo = maybeXml.flatMap(xml => (xml \ "bag-info").theSeq.headOption)
  } yield maybeBagInfo.map(_.toOneLiner)

  private def findBagInfo(doi: String): Try[Option[String]] = Try {
    execute(doi)
  }.recoverWith {
    case t: Throwable => Failure(BagIndexException(s"DOI[$doi] url[$url]" + t.getMessage, t))
  }.map {
    case response if response.code == 404 => None
    case response if response.code == 200 => Some(response.body)
    case response =>
      throw BagIndexException(s"Not expected response code from bag-index. url='${ url }', doi='$doi', response: ${ response.code } - ${ response.body }", null)
  }

  protected def execute(doi: String): HttpResponse[String] = {
    Http(url.toString)
      .param("doi", doi)
      .header("Accept", "text/xml")
      .asString
  }
}
