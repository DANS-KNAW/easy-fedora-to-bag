/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
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

import java.text.SimpleDateFormat

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import nl.knaw.dans.pf.language.emd.types.{ BasicDate, IsoDate }

import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def apply(emdNode: Node): Try[Elem] = Try {
    new EmdUnmarshaller(classOf[EasyMetadataImpl]).unmarshal(emdNode.serialize)
  }.map { emd =>

    // a null value skips rendering the attribute
    val lang: String = emd.getEmdLanguage.getDcLanguage.asScala.headOption.map(_.getValue).orNull
    val descriptions = emd.getEmdDescription.getDcDescription.asScala ++
      emd.getEmdDescription.getTermsAbstract.asScala ++
      emd.getEmdDescription.getTermsTableOfContents.asScala
    val allDates: Map[DatasetId, Iterable[Elem]] = {
      val basicDates = emd.getEmdDate.getAllBasicDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
      val isoDates = emd.getEmdDate.getAllIsoDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
      (basicDates.toSeq ++ isoDates.toSeq)
        .filter(kv => !Seq("created", "available").contains(kv._1))
        .groupBy(_._1)
        .mapValues(_.toMap.values.flatten)
        .map{case (key, values) => key -> values.map(_.withLabel(key))}
    }
    val dateCreated = allDates.filterKeys(_ == "created").values.flatten

    <ddm:DDM
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:dcterms="http://purl.org/dc/terms/"
      xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
      xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
      xmlns:gml="http://www.opengis.net/gml"
      xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
      xmlns:ddm={schemaNameSpace}
      xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
      xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}
    >
      <ddm:profile>
        { emd.getEmdTitle.getDcTitle.asScala.map(bs => <dc:title xml:lang={ lang }>{ bs.getValue }</dc:title>) }
        { descriptions.map(bs => <dcterms:description xml:lang={ lang }>{ bs.getValue }</dcterms:description>) }
        { /* TODO instructions for reuse */ }
        { /* TODO creators */ }
        { dateCreated }
        { allDates.filterKeys(_ == "available").values.flatten.headOption.getOrElse(dateCreated) }
        { emd.getEmdAudience.getDisciplines.asScala.map(bs => <ddm:audience>{ bs.getValue }</ddm:audience>) }
        <ddm:accessRights>{ emd.getEmdRights.getAccessCategory }</ddm:accessRights>
      </ddm:profile>
      <ddm:dcmiMetadata>
        { emd.getEmdIdentifier.getDcIdentifier.asScala.map(bi => <dcterms:identifier xsi:type={ Option(bi.getScheme).orNull }>{ bi.getValue }</dcterms:identifier>) }
        { emd.getEmdTitle.getTermsAlternative.asScala.map(str => <dcterms:alternative xml:lang={ lang }>{ str }</dcterms:alternative>) }
        { /* TODO ... */ }
        { allDates.filterKeys(!Seq("created", "available").contains(_)).values.flatten }
        { /* TODO ... */ }
      </ddm:dcmiMetadata>
    </ddm:DDM>
  }

  private def toXml(value: IsoDate) = <label xsi:type={ value.getScheme.toString }>{ value.getValueAsString }</label>

  private def toXml(value: BasicDate) = <label xsi:type={ value.getScheme.toString }>{ value.getDateTime.toString("yyyy-mm-dd") }</label>

  /** @param elem XML element to be adjusted */
  private implicit class RichElem(val elem: Elem) extends AnyVal {
    /** @param str the desired label, optionally with name space prefix */
    def withLabel(str: String): Elem = {
      str.split(":", 2) match {
        case Array(label) if label.nonEmpty => elem.copy(label = label)
        case Array(prefix, label) => elem.copy(prefix = prefix, label = label)
      }
    }
  }
}
