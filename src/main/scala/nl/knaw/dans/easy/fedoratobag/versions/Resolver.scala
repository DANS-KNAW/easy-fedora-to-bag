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
package nl.knaw.dans.easy.fedoratobag.versions

import scalaj.http.Http

import scala.util.{ Failure, Success, Try }

case class Resolver() {

  def getDatasetId(id: String): Try[String] = {
    id.slice(0, 3) match {
      case "eas" => Success(id)
      case "urn" => resolve(s"http://www.persistent-identifier.nl/?identifier=$id")
      case "10." => resolve(s"https://doi.org/$id")
      case id => Failure(new IllegalArgumentException(s"not expected type of ID for a dataset: $id"))
    }
  }

  private def resolve(url: String) = {
    Try(Http(url).asString).map {
      case response if response.code == 302 =>
        response
          .header("Location")
          .map(_.replaceAll(".*/", "").replace("%3A", ":"))
          .getOrElse(throw new Exception(s"no location header returned by $url - ${ response.body }"))
      case response =>
        throw new Exception(s"Not expected response code from '$url' ${ response.code } - ${ response.body }", null)
    }
  }
}
