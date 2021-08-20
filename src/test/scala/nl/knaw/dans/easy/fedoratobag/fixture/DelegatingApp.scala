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
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.fedoratobag.filter.BagIndex
import nl.knaw.dans.easy.fedoratobag.{ BagVersion, Configuration, DatasetId, DatasetInfo, EasyFedoraToBagApp, FedoraProvider, Options }
import org.scalamock.scalatest.MockFactory

import java.net.URI
import javax.naming.ldap.InitialLdapContext
import scala.util.Try

trait DelegatingApp extends MockFactory {

  /* delegate most of createBag to a mock to test the rest of the class and/or application */
  def delegatingApp(staging: File, createBagExpects: Seq[(String, Try[DatasetInfo])]): EasyFedoraToBagApp = new EasyFedoraToBagApp(
    new Configuration("testVersion", null, null, null, new URI(""), staging, null)
  ) {
    // mock requires a constructor without parameters
    class MockEasyFedoraToBagApp() extends EasyFedoraToBagApp(null) {
      override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
      override lazy val ldapContext: InitialLdapContext = mock[InitialLdapContext]
      override lazy val bagIndex: BagIndex = mock[BagIndex]
    }

    private val delegate = mock[MockEasyFedoraToBagApp]
    createBagExpects.foreach { case (id, result) =>
      (delegate.createBag(_: DatasetId, _: File, _: Options, _: Option[BagVersion])
        ) expects(id, *, *, *) returning result
    }

    override def createBag(datasetId: DatasetId, bagDir: File, options: Options, maybeFirstBagVersion: Option[BagVersion] = None): Try[DatasetInfo] = {
      // mimic a part of the real method, the tested caller wants to move the bag
      DansV0Bag.empty(bagDir).map { bag =>
        maybeFirstBagVersion.foreach(_.addTo(bag))
        bag.save()
      }.getOrElse(s"mock of createBag failed for $datasetId")
      // mock the outcome of the method
      delegate.createBag(datasetId, bagDir, options, maybeFirstBagVersion)
    }
  }
}
