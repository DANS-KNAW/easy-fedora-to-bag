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
package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.common.lang.dataset.AccessCategory._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._
import nl.knaw.dans.pf.language.emd.types.EmdConstants.DateScheme
import nl.knaw.dans.pf.language.emd.types._
import nl.knaw.dans.pf.language.emd.{ EasyMetadataImpl, EmdRights }

import java.net.URI
import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

object DDM extends DebugEnhancedLogging {
  val schemaNameSpace: String = "http://easy.dans.knaw.nl/schemas/md/ddm/"
  val schemaLocation: String = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
  val dansLicense = "http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf"
  val cc0 = "http://creativecommons.org/publicdomain/zero/1.0"

  def apply(emd: EasyMetadataImpl, audiences: Seq[String], abrMapping: AbrMappings): Try[Elem] = Try {
    //    println(new EmdMarshaller(emd).getXmlString)

    val dateMap: Map[String, Iterable[Elem]] = getDateMap(emd)
    val dateCreated = dateMap("created").map(_.text)
    val dateAvailable = {
      val elems = dateMap("available").map(_.text)
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
       { emd.getEmdTitle.getDcTitle.asScala.headOption.toSeq.map(bs => <dc:title xml:lang={ lang(bs) }>{ bs.getValue.trim }</dc:title>) }
       { emd.getEmdDescription.getDcDescription.asScala.map(bs => <dct:description xml:lang={ lang(bs) }>{ bs.getValue.trim }</dct:description>) }
       { /* instructions for reuse not specified as such in EMD */ }
       { emd.getEmdCreator.getDcCreator.asScala.map(bs => <dc:creator>{ bs.getValue.trim }</dc:creator>) }
       { emd.getEmdCreator.getEasCreator.asScala.map(author => <dcx-dai:creatorDetails>{ toXml(author)} </dcx-dai:creatorDetails>) }
       { if (dateCreated.nonEmpty)  <ddm:created>{ dateCreated.toSeq.head }</ddm:created> }
       { dateAvailable.map(date =>  <ddm:available>{ date }</ddm:available>) }
       { audiences.map(code => <ddm:audience>{ code }</ddm:audience>) }
       <ddm:accessRights>{ emd.getEmdRights.getAccessCategory }</ddm:accessRights>
     </ddm:profile>
     <ddm:dcmiMetadata>
       { emd.getEmdIdentifier.getDcIdentifier.asScala.map(bi => <dct:identifier xsi:type={ idType(bi) }>{ bi.getValue.trim }</dct:identifier>) }
       { emd.getEmdTitle.getDcTitle.asScala.drop(1).map(bs => <dc:title xml:lang={ lang(bs) }>{ bs.getValue.trim }</dc:title>) }
       { emd.getEmdTitle.getTermsAlternative.asScala.map(str => <dct:alternative>{ str }</dct:alternative>) }
       { emd.getEmdDescription.getTermsAbstract.asScala.map(bs => <ddm:description xml:lang={ lang(bs) } descriptionType='Abstract'>{ bs.getValue.trim }</ddm:description>) }
       { emd.getEmdDescription.getTermsTableOfContents.asScala.map(bs => <ddm:description xml:lang={ lang(bs) } descriptionType='TableOfContents'>{ bs.getValue.trim }</ddm:description>) }
       { emd.getEmdRelation.getDCRelationMap.asScala.map { case (key, values) => values.asScala.map(toRelationXml(key, _)) } }
       { emd.getEmdRelation.getRelationMap.asScala.map { case (key, values) => values.asScala.map(toRelationXml(key, _)) } }
       { emd.getEmdContributor.getDcContributor.asScala.map(bs => <dc:contributor>{ bs.getValue.trim }</dc:contributor>) }
       { emd.getEmdContributor.getEasContributor.asScala.map(author => <dcx-dai:contributorDetails>{ toXml(author)} </dcx-dai:contributorDetails>) }
       { emd.getEmdRights.getTermsRightsHolder.asScala.map(bs => <dct:rightsHolder>{ bs.toString }</dct:rightsHolder>) }
       { emd.getEmdPublisher.getDcPublisher.asScala.map(bs => <dct:publisher xml:lang={ lang(bs) }>{ bs.getValue.trim }</dct:publisher>) }
       { emd.getEmdSource.getDcSource.asScala.map(bs => <dc:source xml:lang={ lang(bs) }>{ bs.getValue.trim }</dc:source>) }
       { emd.getEmdType.getDcType.asScala.map(bs => <dct:type xsi:type={ xsiType(bs) }>{ bs.getValue.trim }</dct:type>) }
       { emd.getEmdFormat.getDcFormat.asScala.map(bs => <dct:format>{ bs.getValue.trim }</dct:format>) }
       { emd.getEmdFormat.getTermsExtent.asScala.map(bs => <dct:extent>{ bs.getValue.trim }</dct:extent>) }
       { emd.getEmdFormat.getTermsMedium.asScala.map(bs => <dct:medium>{ bs.getValue.trim }</dct:medium>) }
       { emd.getEmdSubject.getDcSubject.asScala.map(toXml("subject", abrMapping.subject)) }
       { emd.getEmdCoverage.getDcCoverage.asScala.map(bs => <dct:coverage xml:lang={ lang(bs) }>{ bs.getValue.trim }</dct:coverage>) }
       { emd.getEmdCoverage.getTermsSpatial.asScala.map(bs => <dct:spatial xml:lang={ lang(bs) } xsi:type={ xsiType(bs) }>{ bs.getValue.trim }</dct:spatial>) }
       { emd.getEmdCoverage.getTermsTemporal.asScala.map(toXml("temporal", abrMapping.temporal)) }
       { dateMap.filter(isOtherDate).map { case (key, values) => values.map(_.withLabel(dateLabel(key))) } }
       { emd.getEmdCoverage.getEasSpatial.asScala.map(toXml) }
       <dct:license xsi:type="dct:URI">{ toLicenseUrl(emd.getEmdRights) }</dct:license>
       { emd.getEmdLanguage.getDcLanguage.asScala.map(bs => <dct:language xsi:type={langType(bs)}>{ langValue(bs) }</dct:language>) }
       { dateCreated.toSeq.drop(1).map(date => <dct:created>{ date }</dct:created>) }
     </ddm:dcmiMetadata>
   </ddm:DDM>
 }

  private def langType(bs: BasicString): String = bs.getSchemeId match {
    case "fra" | "fra/fre" | "deu" | "deu/ger" | "nld" | "nld/dut" | "dut/nld" | "eng" => "dct:ISO639-3"
    case _ => null
  }

  private def langValue(bs: BasicString): String = bs.getValue.trim match {
    case "fra/fre" => "fra"
    case "deu/ger" => "deu"
    case "nld/dut" | "dut/nld" => "nld"
    case s => s
  }

  private def toXml(label: String, abrMapping: Map[String, Elem])(bs: BasicString): Elem = {
    if (bs.getScheme == "ABR" && bs.getSchemeId.matches(s"archaeology.dc.*$label"))
      abrMapping.get(bs.getValue)
    else None
  }.getOrElse {
    <dct:label xml:lang={ lang(bs) } xsi:type={ xsiType(bs) }>{ bs.getValue.trim }</dct:label>
      .withLabel(s"dct:$label")
  }

  private def xsiType(bs: BasicString): String = {
    val scheme = Option(bs.getScheme).map(_.toUpperCase())
    (scheme, Option(bs.getSchemeId)) match {
      case (Some("ABR"), Some("archaeology.dc.subject")) => "abr:ABRcomplex"
      case (Some("ABR"), Some("archaeology.dct.subject")) => "abr:ABRcomplex"
      case (Some("ABR"), Some("archaeology.dcterms.subject")) => "abr:ABRcomplex"
      case (Some("ABR"), Some("archaeology.dc.temporal")) => "abr:ABRperiode"
      case (Some("ABR"), Some("archaeology.dct.temporal")) => "abr:ABRperiode"
      case (Some("ABR"), Some("archaeology.dcterms.temporal")) => "abr:ABRperiode"
      case (Some("ABR"), _) => notImplementedAttribute("ABR schemeId")(bs)
      case (Some("DCMI"), Some("common.dc.type")) => "dct:DCMIType"
      case (Some("DCMI"), Some("common.dct.type")) => "dct:DCMIType"
      case (Some("DCMI"), Some("common.dcterms.type")) => "dct:DCMIType"
      case (Some("DCMI"), _) => notImplementedAttribute("DCMI schemeId")(bs)
      case (_, Some(scheme)) if scheme.startsWith("id-type:") => scheme
      case (None, None) => null
      case _ => notImplementedAttribute("")(bs)
    }
  }

  private val schemeToIdType = Map(
    "PID" -> "URN",
    "Archis_onderzoek_m_nr" -> "ARCHIS-ZAAK-IDENTIFICATIE",
    "DOI" -> "DOI",
    "DMO_ID" -> "EASY2",
    "AIP_ID" -> "EASY1",
    "ISBN" -> "ISBN",
    "ISNI" -> "ISNI",
    "ISSN" -> "ISSN",
    "DOI_OTHER_ACCESS" -> "DOI",
    "eDNA-project" -> "eDNA-project",
    "Archis_onderzoek" -> "ARCHIS-ONDERZOEK",
    "Archis_vondstmelding" -> "ARCHIS-VONDSTMELDING",
    "Archis_art.41" -> "ARCHIS-ZAAK-IDENTIFICATIE",
    "Archis_waarneming" -> "ARCHIS-WAARNEMING",
    "Archis_monument" -> "ARCHIS-MONUMENT",
    "NWO-projectnummer" -> "NWO-PROJECTNR",
  )

  private def idType(bs: BasicString): DatasetId = Option(bs.getScheme)
    .filterNot(_.trim.isEmpty)
    .flatMap(schemeToIdType.get)
    .map("id-type:" + _)
    .orNull // meaning: or omit attribute

  private def notImplementedAttribute(msg: String)(data: Any): String = {
    logger.error(s"not implemented $msg [$data]")
    // schema validation will stumble with "cvc-datatype-valid.1.2.1: '-' is not a valid value for 'QName'."
    "-"
  }

  private def notImplemented(msg: String)(data: Any): Elem = {
    logger.error(s"not implemented $msg [$data]")
    <not:implemented/>
  }

  /** a null value skips rendering the attribute */
  private def lang(bs: BasicString): String = Option(bs.getLanguage).map(_.replace("/", "-")).orNull

  private def toXml(author: Author): Seq[Node] = {
    val surname = author.getSurname
    val maybeRole = Option(author.getRole).map(_.getRole).filter(_.trim.nonEmpty)
    if (surname == null || surname.trim.isEmpty)
      Option(author.getOrganization).toSeq.map(toXml(_, maybeRole))
    else
      <dcx-dai:author>
        { seq(author.getTitle).map(str => <dcx-dai:titles>{ str }</dcx-dai:titles>) }
        { seq(author.getInitials).map(str => <dcx-dai:initials>{ str }</dcx-dai:initials>) }
        { seq(author.getPrefix).map(str => <dcx-dai:insertions>{ str }</dcx-dai:insertions>) }
        <dcx-dai:surname>{ surname }</dcx-dai:surname>
        { Option(author.getOrcid).toSeq.map(id => <dcx-dai:ORCID>{ toURI(id) }</dcx-dai:ORCID>) }
        { Option(author.getIsni).toSeq.map(id => <dcx-dai:ISNI>{ toURI(id) }</dcx-dai:ISNI>) }
        { Option(author.getDigitalAuthorId).toSeq.map(dai => <dcx-dai:DAI>{ dai.getURI }</dcx-dai:DAI>) }
        { maybeRole.toSeq.map(str => <dcx-dai:role>{ str }</dcx-dai:role>) }
        { seq(author.getOrganization).map(toXml(_, maybeRole = None)) }
      </dcx-dai:author>
  }

  private def toXml(organization: String, maybeRole: Option[String]): Elem =
      <dcx-dai:organization>
        { <dcx-dai:name>{ organization }</dcx-dai:name> }
        { maybeRole.toSeq.map(str => <dcx-dai:role>{ str }</dcx-dai:role>) }
      </dcx-dai:organization>

  private def toURI(id: EntityId): String = {
    val uri = id.getIdentificationSystem.toString.replaceAll("/*$", "")
    s"$uri/${ id.getEntityId }"
  }

  private def toXml(spatial: Spatial): Node = {
    (Option(spatial.getPlace),
      Option(spatial.getPoint),
      Option(spatial.getBox),
      Option(spatial.getPolygons),
    ) match {
      case (None, None, None, Some(polygons)) if !polygons.isEmpty =>
        <dcx-gml:spatial>
          { toXml(polygons.asScala) }
        </dcx-gml:spatial>
      case (_, Some(_), None, None) => toXmlPoint(spatial.getPoint)
      case (None, None, Some(_), None) => toXml(spatial.getBox)
      case _ => notImplemented("expected either point, box or polygon")(spatial)
    }
  }

  private def toXmlPoint(emdPoint: Spatial.Point) = {
    val point = SpatialPoint(
      optional(emdPoint.getScheme),
      optional(emdPoint.getX),
      optional(emdPoint.getY),
    )
    point.dcxGml.getOrElse {
      logger.warn(s"Empty point: $emdPoint")
      Text("")
    }
  }

  private def toXml(spatial: Spatial.Box): Elem = {
    val box = SpatialBox(
      optional(spatial.getScheme),
      optional(spatial.getNorth),
      optional(spatial.getEast),
      optional(spatial.getSouth),
      optional(spatial.getWest),
    )
    box.dcxGml.getOrElse(notImplemented("invalid box")(box))
  }

  private def toXml(polygons: Seq[Polygon]): Seq[Node] = polygons.map { polygon =>
    val maybeScheme: Option[String] = optional(polygon.getScheme)
    val srsName = new SchemedSpatial {
      override val scheme: Option[String] = maybeScheme
      override val value: Option[String] = None
    }.srsName
    val place = optional(polygon.getPlace)
      .map(place => <name>{ place }</name>)
      .getOrElse(Text(""))
    val exterior = Option(polygon.getExterior)
      .map(part => <exterior>{ toXml(part, maybeScheme) }</exterior>)
      .getOrElse(Text(""))
    val interiors = Option(polygon.getInterior).toSeq.flatMap(_.asScala)
      .flatMap(part => <interior>{ toXml(part, maybeScheme) }</interior>)
    <Polygon xmlns='http://www.opengis.net/gml' srsName={ srsName }>
        { place }
        { exterior }
        { interiors }
    </Polygon>
  }

  private def toXml(part: PolygonPart, maybeScheme: Option[String]): Node = {
    val place = optional(part.getPlace)
      .map(s => <description>{ s }</description>)
      .getOrElse(Text(""))
    val points = part.getPoints.asScala.map { polygonPoint =>
      val x = optional(polygonPoint.getX)
      val y = optional(polygonPoint.getY)
      SpatialPoint(maybeScheme, x, y).pos
    }
    <LinearRing>
        { place }
        { <posList>{ points.mkString(" ") }</posList> }
    </LinearRing>
  }

  private def toXml(value: IsoDate): Elem = <label xsi:type={ orNull(value.getScheme) }>{ fixDate(value) }</label>

  private def toXml(value: BasicDate): Elem = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  def orNull(dateScheme: DateScheme): String = Option(dateScheme).map("dct:" + _.toString).orNull

  private def optional(s: String) = Option(s).filterNot(_.trim.isEmpty)

  def orNull(s: String): String = optional(s).orNull

  private def isOtherDate(kv: (String, Iterable[Elem])): Boolean = !Seq("created", "available").contains(kv._1)

  private def dateLabel(key: String): String = key.toOption.map("dct:" + _).getOrElse("dct:date")

  private def getDateMap(emd: EasyMetadataImpl): Map[DatasetId, Seq[Elem]] = {
    val basicDates = emd.getEmdDate.getAllBasicDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
    val isoDates = emd.getEmdDate.getAllIsoDates.asScala.map { case (key, values) => key -> values.asScala.map(toXml) }
    (basicDates.toSeq ++ isoDates.toSeq)
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))
  }

  private def fixDate(date: IsoDate) = {
    val year = date.getValue.getYear
    if (year <= 9999) date
    else {
      // some dates where stored as yyyymmdd-01-01
      val dateTime = date.getValue
        .withYear(year / 10000)
        .withMonthOfYear(year % 10000 / 100)
        .withDayOfMonth(year % 100)
      date.setValue(dateTime)
      date
    }
  }.toString.replaceAll("[+]([0-9][0-9])([0-9][0-9])", "+$1:$2") // fix time zone

  private def toRelationXml(key: String, rel: Relation): Elem = Try {
    {
      <label scheme={ relationType(rel) }
             href={ Option(rel.getSubjectLink).map(toHref).orNull }
             xml:lang={ Option(rel.getSubjectTitle).map(_.getLanguage).orNull }
      >{
        val optionTitle = Option(rel.getSubjectTitle).getOrElse(new BasicString()).getValue.toOption
        optionTitle.map(_.trim)
          .getOrElse(rel.getSubjectLink)
        }</label>
    }.withLabel(relationLabel("ddm:", key))
  }.getOrElse(notImplemented(s"relation ($key)")(rel))

  private def toHref(uri: URI) = {
    Option(uri.getScheme).map {
      case "urn" => "https://persistent-identifier.nl/" + uri
      case "doi" => "https://doi.org/" + uri.getSchemeSpecificPart
      case "http" | "https" => uri.toString
      case _ => ??? // recover with Try{...}.getOrElse of caller to fail slow
    }.getOrElse {
      if (uri.toString.startsWith("10.17026/")) s"https://doi.org/$uri"
      else if (uri.toString.isBlank) null
           else uri.toString
    }
  }

  private def toRelationXml(key: String, bs: BasicString): Node = {
    if (bs.getScheme == "STREAMING_SURROGATE_RELATION") {
      logger.info(s"skipped ${ relationLabel("dct:", key) } ${ bs.getScheme } $bs")
      Text("")
    }
    else <label xsi:type={ idType(bs) }
                xml:lang={ bs.getLanguage }
         >{ bs.getValue.trim }</label>.withLabel(relationLabel("dct:", key))
  }

  private def relationType(rel: Relation): String = {
    Option(rel.getSubjectLink).map(_.getAuthority match {
      case "persistent-identifier.nl" => "id-type:URN"
      case "doi.org" => "id-type:DOI"
      case _ => null
    }).orNull // omits the attribute
  }

  private def relationLabel(prefix: String, key: String): String = prefix + {
    key.toOption.getOrElse("relation")
  }

  /** @return an empty Seq for a null or blank String */
  private def seq(s: String): Seq[String] = Option(s).flatMap(_.trim.toOption).toSeq

  private def toLicenseUrl(emdRights: EmdRights) = {
    emdRights.getTermsLicense.asScala
      .find(_.getValue.trim.startsWith("http"))
      .getOrElse(emdRights.getAccessCategory match {
        case ANONYMOUS_ACCESS | OPEN_ACCESS | FREELY_AVAILABLE | OPEN_ACCESS_FOR_REGISTERED_USERS => cc0
        case _ => dansLicense
      })
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
