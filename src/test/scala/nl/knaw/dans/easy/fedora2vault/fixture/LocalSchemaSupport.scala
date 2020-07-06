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

import better.files.{ File, StringExtensions }
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import nl.knaw.dans.easy.fedora2vault.XmlExtensions

import scala.util.{ Failure, Try }
import scala.xml.Node

trait LocalSchemaSupport {
  val schema: String
  private lazy val triedSchema = Try {
    // lazy for two reasons:
    // - schemaFile is set by concrete test class
    // - postpone loading until actually validating
    val xsdInputStream = (File("target/easy-schema") / schema)
      .contentAsString.inputStream
    SchemaFactory
      .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      .newSchema(Array[Source](new StreamSource(xsdInputStream)))
  }.recoverWith { case t =>
    Failure(new Exception(s"Could not load schema [$schema] $t", t))
  }

  def validate(xml: Node): Try[Unit] = {
    val serialized = xml.serialize
    triedSchema.flatMap { schema =>
      val source = new StreamSource(serialized.inputStream)
      Try(schema.newValidator().validate(source))
    }
  }
}
