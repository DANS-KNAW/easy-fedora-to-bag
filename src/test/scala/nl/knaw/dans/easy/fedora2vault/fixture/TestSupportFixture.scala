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
package nl.knaw.dans.easy.fedora2vault.fixture

import better.files.File
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait TestSupportFixture extends AnyFlatSpec with TimeZoneFixture

  with Matchers
  with Inside
  with OptionValues
  with EitherValues
  with Inspectors {
  val nameSpaceRegExp = """ xmlns:[a-z-]+="[^"]*"""" // these attributes have a variable order
  val sampleFoXML: File = File("src/test/resources/sample-foxml")

  val emdNS = "http://easy.dans.knaw.nl/easy/easymetadata/"
  val easNS = "http://easy.dans.knaw.nl/easy/easymetadata/eas/"
  val dctNS = "http://purl.org/dc/terms/"
  val dcNS = "http://purl.org/dc/elements/1.1/"
}
