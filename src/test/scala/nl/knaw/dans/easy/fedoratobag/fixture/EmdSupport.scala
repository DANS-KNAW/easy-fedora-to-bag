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
package nl.knaw.dans.easy.fedoratobag.fixture

import better.files.File
import nl.knaw.dans.easy.fedoratobag._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.scalatest.Assertions._

import scala.util.Try
import scala.xml.{ Elem, NodeSeq, Utility }

trait EmdSupport {
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])
  val abrMapping: AbrMappings = AbrMappings(File("src/main/assembly/dist/cfg/EMD_acdm.xsl"))


  def parseEmdContent(xml: NodeSeq): EasyMetadataImpl = {
    val emdXml = <emd:easymetadata xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/"
                          xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/"
                          xmlns:dct="http://purl.org/dc/terms/"
                          xmlns:dc="http://purl.org/dc/elements/1.1/"
                          emd:version="0.1"
        >{ xml }</emd:easymetadata>
    // TODO implement like https://github.com/DANS-KNAW/easy-fedora-to-bag/blob/4a4b7689e44aa0bb9c4c45dee8123899bfcd3de5/src/main/scala/nl/knaw/dans/easy/fedoratobag/EasyFedoraToBagApp.scala#L188
    //   but that breaks https://github.com/DANS-KNAW/easy-fedora-to-bag/blob/2a47e60e8a267c00e1863f3e6e72172d4abd2e8f/src/test/scala/nl/knaw/dans/easy/fedoratobag/DdmSpec.scala#L952
    Try(emdUnmarshaller.unmarshal( printer.format(Utility.trim(emdXml))))
      .getOrRecover(e => fail("could not load test EMD", e))
  }

  def emd2ddm(emd: EasyMetadataImpl): Elem = {
    DDM(emd, Seq.empty, abrMapping)
      .getOrRecover(e => fail("could not create DDM from test EMD", e))
  }
}
