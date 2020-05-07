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

import nl.knaw.dans.common.lang.dataset.AccessCategory._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import nl.knaw.dans.pf.language.emd.types.EmdConstants.DateScheme
import nl.knaw.dans.pf.language.emd.types._
import nl.knaw.dans.pf.language.emd.{ EasyMetadataImpl, EmdRights }

import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val dansLicense = "http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf"
  val cc0 = "http://creativecommons.org/publicdomain/zero/1.0"

  def apply(emd: EasyMetadataImpl, audiences: Seq[String]): Try[Elem] = Try {
    //    println(new EmdMarshaller(emd).getXmlString)

    val dateMap: Map[String, Iterable[Elem]] = getDateMap(emd)
    val dateCreated = dateMap("created")
    val dateAvailable = {
      val elems = dateMap("available")
      if (elems.isEmpty) dateCreated
      else elems
    }
   <ddm:DDM
     xmlns:dc="http://purl.org/dc/elements/1.1/"
     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xmlns:dct="http://purl.org/dc/terms/"
     xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
     xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
     xmlns:gml="http://www.opengis.net/gml"
     xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
     xmlns:ddm={schemaNameSpace}
     xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
     xsi:schemaLocation={s"$schemaNameSpace $schemaLocation"}
   >
     <ddm:profile>
       { emd.getEmdTitle.getDcTitle.asScala.map(bs => <dc:title xml:lang={ lang(bs) }>{ bs.getValue }</dc:title>) }
       { emd.getEmdDescription.getDcDescription.asScala.map(bs => <dct:description xml:lang={ lang(bs) }>{ bs.getValue }</dct:description>) }
       { /* instructions for reuse not specified as such in EMD */ }
       { emd.getEmdCreator.getDcCreator.asScala.map(bs => <dc:creator>{ bs.getValue }</dc:creator>) }
       { emd.getEmdCreator.getEasCreator.asScala.map(author => <dcx-dai:creatorDetails>{ toXml(author)} </dcx-dai:creatorDetails>) }
       { dateCreated.map(node =>  <ddm:created>{ node.text }</ddm:created>) }
       { dateAvailable.map(node =>  <ddm:available>{ node.text }</ddm:available>) }
       { audiences.map(code => <ddm:audience>{ code }</ddm:audience>) }
       <ddm:accessRights>{ emd.getEmdRights.getAccessCategory }</ddm:accessRights>
     </ddm:profile>
     <ddm:dcmiMetadata>
       { emd.getEmdIdentifier.getDcIdentifier.asScala.filter(isDdmId).map(bi => <dct:identifier xsi:type={ idType(bi) }>{ bi.getValue }</dct:identifier>) }
       { emd.getEmdTitle.getTermsAlternative.asScala.map(str => <dct:alternative>{ str }</dct:alternative>) }
       { emd.getEmdDescription.getTermsAbstract.asScala.map(bs => <ddm:description xml:lang={ lang(bs) } descriptionType='Abstract'>{ bs.getValue }</ddm:description>) }
       { emd.getEmdDescription.getTermsTableOfContents.asScala.map(bs => <ddm:description xml:lang={ lang(bs) } descriptionType='TableOfContent'>{ bs.getValue }</ddm:description>) }
       { emd.getEmdRelation.getDCRelationMap.asScala.map { case (key, values) => values.asScala.map(toRelationXml(key, _)) } }
       { emd.getEmdRelation.getRelationMap.asScala.map { case (key, values) => values.asScala.map(toRelationXml(key, _)) } }
       { emd.getEmdContributor.getDcContributor.asScala.map(bs => <dc:contributor>{ bs.getValue }</dc:contributor>) }
       { emd.getEmdContributor.getEasContributor.asScala.map(author => <dcx-dai:contributorDetails>{ toXml(author)} </dcx-dai:contributorDetails>) }
       { emd.getEmdContributor.getEasContributor.asScala.filter(isRightsHolder).map(notImplemented("rightsHolder")) }
       { emd.getEmdPublisher.getDcPublisher.asScala.map(bs => <dct:publisher xml:lang={ lang(bs) }>{ bs.getValue }</dct:publisher>) }
       { emd.getEmdSource.getDcSource.asScala.map(bs => <dc:source xml:lang={ lang(bs) }>{ bs.getValue }</dc:source>) }
       { emd.getEmdType.getDcType.asScala.map(bs => <dct:type xsi:type={ dcType(bs) }>{ bs.getValue }</dct:type>) }
       { emd.getEmdFormat.getDcFormat.asScala.map(bs => <dct:format>{ bs.getValue }</dct:format>) }
       { emd.getEmdFormat.getTermsExtent.asScala.map(notImplemented("extent format")) }
       { emd.getEmdFormat.getTermsMedium.asScala.map(notImplemented("medium formt")) }
       { emd.getEmdSubject.getDcSubject.asScala.filter(hasSimpleScheme).map(bs => <dc:subject xml:lang={ lang(bs) } xsi:type={ abrType(bs) }>{ bs.getValue }</dc:subject>) }
       { emd.getEmdSubject.getDcSubject.asScala.filterNot(hasSimpleScheme).map(notImplemented("schemed subject")) }
       { emd.getEmdCoverage.getDcCoverage.asScala.map(bs => <dct:coverage xml:lang={ lang(bs) }>{ bs.getValue }</dct:coverage>) }
       { emd.getEmdCoverage.getTermsSpatial.asScala.filter(hasSimpleScheme).map(bs => <dct:spatial xml:lang={ lang(bs) } xsi:type={ abrType(bs) }>{ bs.getValue }</dct:spatial>) }
       { emd.getEmdCoverage.getTermsSpatial.asScala.filterNot(hasSimpleScheme).map(bs => <dct:spatial xml:lang={ lang(bs) } xsi:type={ xsiType(bs) }>{ bs.getValue }</dct:spatial>) }
       { emd.getEmdCoverage.getTermsTemporal.asScala.filter(hasSimpleScheme).map(bs => <dct:temporal xml:lang={ lang(bs) } xsi:type={ abrType(bs) }>{ bs.getValue }</dct:temporal>) }
       { emd.getEmdCoverage.getTermsTemporal.asScala.filterNot(hasSimpleScheme).map(bs => <dct:temporal xml:lang={ lang(bs) } xsi:type={ xsiType(bs) }>{ bs.getValue }</dct:temporal>) }
       { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getPlace == null).map(notImplemented("places")) }
       { dateMap.filter(isOtherDate).map { case (key, values) => values.map(_.withLabel(dateLabel(key))) } }
       { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getBox == null).map(notImplemented("boxes")) }
       { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getPoint == null).map(notImplemented("points")) }
       { emd.getEmdCoverage.getEasSpatial.asScala.filterNot(_.getPolygons == null).map(notImplemented("polygons")) }
       <dct:license xsi:type="dct:URI">{ toLicenseUrl(emd.getEmdRights) }</dct:license>
       { emd.getEmdLanguage.getDcLanguage.asScala.map(bs => <dct:language xsi:type={langType(bs)}>{ langValue(bs) }</dct:language>) }
     </ddm:dcmiMetadata>
   </ddm:DDM>
 }

  private def isRightsHolder(author: Author) = {
    Option(author.getRole).exists(_.getRole == "RightsHolder")
  }

  private def langType(bs: BasicString): String = bs.getSchemeId match {
    case "fra" | "fra/fre" | "deu" | "deu/ger" | "nld" | "nld/dut" | "eng" => "dct:ISO639-3"
    case _ => null
  }

  private def langValue(bs: BasicString): String = bs.getValue match {
    case "fra/fre" => "fra"
    case "deu/ger" => "deu"
    case "nld/dut" => "nld"
    case s => s
  }

  private def hasSimpleScheme(string: BasicString): Boolean = {
    Option(string.getScheme).map(_.toUpperCase()) match {
      case Some("ABR") | None => true
      case _ => false
    }
  }

  private def abrType(bs: BasicString): String = bs.getSchemeId match {
    case "archaeology.dc.subject" => "abr:ABRcomplex"
    case "archaeology.dc.temporal" => "abr:ABRperiode"
    case _ => notImplementedAttribute(s"ABR schemeId")(bs)
      null
  }

  private def dcType(bs: BasicString): String = {
    (bs.getScheme, bs.getSchemeId) match {
      case ("DCMI", "common.dc.type") => "dct:DCMIType"
      case _ => notImplementedAttribute(s"dctType")(bs)
        null
    }
  }

  private def idType(bs: BasicString): String = Option(bs.getScheme).map(s => "id-type:" + s).orNull

  private def xsiType(bs: BasicString): String = {
    if (bs.getScheme != null && bs.getScheme.startsWith("id-type"))
      bs.getScheme
    else if (bs.getScheme != null || bs.getSchemeId != null)
           notImplementedAttribute("")(bs)
    null
  }

  private def notImplementedAttribute(msg: String)(data: Any): Unit = {
    // TODO return something that won't pass validation
    logger.error(s"not implemented $msg [$data]")
  }

  private def notImplemented(msg: String)(data: Any): Elem = {
    logger.error(s"not implemented $msg [$data]")
    <not:implemented/>
  }

  /** a null value skips rendering the attribute */
  private def lang(bs: BasicString): String = Option(bs.getLanguage).map(_.replace("/", "-")).orNull

  private def toXml(author: Author): Seq[Node] = {
    val surname = author.getSurname
    if (surname == null || surname.trim.isEmpty)
      Option(author.getOrganization).toSeq.map(toXml(_, Option(author.getRole)))
    else
      <dcx-dai:author>
        { seq(author.getTitle).map(str => <dcx-dai:titles>{ str }</dcx-dai:titles>) }
        { seq(author.getInitials).map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { seq(author.getPrefix).map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        <dcx-dai:surname>{ surname }</dcx-dai:surname>
        { Option(author.getOrcid).toSeq.map(id => { <dcx-dai:ORCID>{ toURI(id) }</dcx-dai:ORCID> }) }
        { Option(author.getIsni).toSeq.map(id => { <dcx-dai:ISNI>{ toURI(id) }</dcx-dai:ISNI> }) }
        { Option(author.getDigitalAuthorId).toSeq.map(dai => { <dcx-dai:DAI>{ dai.getURI }</dcx-dai:DAI> }) }
        { Option(author.getRole).toSeq.map(role =>  <dcx-dai:role>{ role.getRole }</dcx-dai:role>) }
        { seq(author.getOrganization).map(toXml(_, maybeRole = None)) }
      </dcx-dai:author>
  }

  private def toURI(id: EntityId): String = {
    val uri = id.getIdentificationSystem.toString.replaceAll("/*$", "")
    s"$uri/${ id.getEntityId }"
  }

  private def toXml(value: IsoDate): Elem = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  private def toXml(value: BasicDate): Elem = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  def orNull(dateScheme: DateScheme): String = Option(dateScheme).map("dct:" + _.toString).orNull

  private def isOtherDate(kv: (String, Iterable[Elem])): Boolean = !Seq("created", "available").contains(kv._1)

  private def dateLabel(key: String): String = {
    key.toOption.map("dct:" + _).getOrElse("dct:date")
  }

  private def getDateMap(emd: EasyMetadataImpl): Map[DatasetId, Seq[Elem]] = {
    val basicDates = emd.getEmdDate.getAllBasicDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
    val isoDates = emd.getEmdDate.getAllIsoDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
    (basicDates.toSeq ++ isoDates.toSeq)
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))
  }

  private def getAudience(id: String)(implicit fedoraProvider: FedoraProvider): Try[String] = {
    fedoraProvider.loadFoXml(id).map(foXml =>
      (foXml \\ "discipline-md" \ "OICode").text
    )
  }

  private def toRelationXml(key: String, rel: Relation): Elem = {
    <label scheme={ relationType(rel) }
           href={ rel.getSubjectLink.toURL.toString }
           xml:lang={ rel.getSubjectTitle.getLanguage }
    >{ rel.getSubjectTitle.getValue }</label>
  }.withLabel(relationLabel("ddm:", key))

  private def toRelationXml(key: String, bs: BasicString): Elem = {
    if (bs.getScheme == "STREAMING_SURROGATE_RELATION") notImplemented("relation")(bs)
    else <label xsi:type={ idType(bs) }
                xml:lang={ bs.getLanguage }
         >{ bs.getValue }</label>
  }.withLabel(relationLabel("dct:", key))

  private def relationType(rel: Relation): String = {
    rel.getSubjectLink.getAuthority match {
      case "persistent-identifier.nl" => "id-type:URN"
      case "doi.org" => "id-type:DOI"
      case _ => null
    }
  }

  private def relationLabel(prefix: String, key: String): String = prefix + {
    key.toOption.getOrElse("relation")
  }

  /** @return an empty Seq for a null or blank String */
  private def seq(s: String): Seq[String] = Option(s).flatMap(_.trim.toOption).toSeq

  private def toXml(organization: String, maybeRole: Option[Author.Role]): Elem =
      <dcx-dai:organization>
        { <dcx-dai:name>{ organization }</dcx-dai:name> }
        { maybeRole.toSeq.map(role => <dcx-dai:role>{ role.getRole }</dcx-dai:role>) }
      </dcx-dai:organization>

  private def toLicenseUrl(emdRights: EmdRights) = {
    emdRights.getTermsLicense.asScala
      .find(_.getValue.startsWith("http"))
      .getOrElse(emdRights.getAccessCategory match {
        case ANONYMOUS_ACCESS | OPEN_ACCESS | FREELY_AVAILABLE | OPEN_ACCESS_FOR_REGISTERED_USERS => cc0
        case _ => dansLicense
      })
  }

  private def isDdmId(bi: BasicIdentifier): Boolean = {
    // these ID's are generated by easy-ingest-flow/EASY-I, not converted from DDM
    !Seq("PID", "DMO_ID", "AIP_ID").contains(bi.getScheme)
  }

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
