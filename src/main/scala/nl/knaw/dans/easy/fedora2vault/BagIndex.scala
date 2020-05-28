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

import scalaj.http.Http

import scala.util.{ Failure, Try }

case class BagIndex(bagIndexUri: URI) {

  /** An IOException is fatal for the batch of datasets */
  private case class BagIndexException(msg: String, cause: Throwable) extends IOException(msg, cause)

  private val url: URI = bagIndexUri.resolve("search")

  def bagByDoi(doi: String): Try[Option[String]] = Try {
    Http(url.toString)
      .param("doi", doi)
      .header("Accept", "text/xml")
      .asString
  }.recoverWith {
    case t: Throwable => Failure(BagIndexException(s"DOI[$doi] url[$url]" + t.getMessage, t))
  }.map {
    case response if response.code == 400 => None
    case response if response.code == 200 => Some(s"$doi: ${response.body}")
    case response => throw BagIndexException(s"Not expected response code from bag-index. url='${ url } }', doi='$doi', response: ${ response.code } - ${ response.body }", null)
  }
}
