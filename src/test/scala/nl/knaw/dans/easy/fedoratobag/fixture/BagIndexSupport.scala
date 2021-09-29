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
package nl.knaw.dans.easy.fedoratobag.fixture

import java.net.URI

import nl.knaw.dans.easy.fedoratobag.filter.BagIndex
import scalaj.http.HttpResponse

trait BagIndexSupport {
  /**
   * Limited to test scenarios where the BagIndex service
   * always gives the the same response
   */
  def mockBagIndexRespondsWith(body: String, code: Int): BagIndex = {
    new BagIndex(new URI("https://does.not.exist.dans.knaw.nl:20120")) {
      override def execute(doi: String): HttpResponse[String] = {
        new HttpResponse[String](body, code, headers = Map.empty)
      }
    }
  }
}
