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
package nl.knaw.dans.easy.fedoratobag

import better.files.File

import scala.xml.{ Elem, Node, XML }

case class AbrMappings(temporal: Map[String, Elem],
                       subject: Map[String, Elem],
                      )

object AbrMappings {
  def apply(acdmFile: File): AbrMappings = {
    val acdmXml = XML.loadFile(acdmFile.toJava)
    new AbrMappings(
      (acdmXml \ "periods" \ "period").theSeq.map(toTemporal).toMap,
      (acdmXml \ "complexlist" \ "complex").theSeq.map(toSubject).toMap,
    )
  }

  private def toTemporal(node: Node) = {
    val key = (node \ "code").text
    key -> <ddm:temporal xml:lang="en"
                         valueURI={ (node \ "uri").text.trim }
                         subjectScheme="Archeologisch Basis Register"
                         schemeURI="http://www.rnaproject.org"
           >{ s"${ (node \ "name").text.trim } ($key)" }</ddm:temporal>
  }

  private def toSubject(node: Node) = {
    val key = (node \ "code").text
    key -> <ddm:subject xml:lang="nl"
                        valueURI={ (node \ "uri").text.trim }
                        subjectScheme="Archeologisch Basis Register"
                        schemeURI="http://www.rnaproject.org"
           >{ s"${ (node \ "label").text.trim } ($key)" }</ddm:subject>
  }
}
