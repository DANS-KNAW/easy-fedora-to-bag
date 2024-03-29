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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try
import scala.xml.Elem

// TODO copied from easy-deposit-api
//  which in turn was a (better isolated) variation of
//  easy-split-multi-deposit/AddDatasetMetadataToDeposit
//  * dropped the trait OptionalValue from easy-deposit-api
//  * added an dcxGml field as that also is a common factor in these 3 projects
//    it won't be of use for DDM to dataverse-json

object SpatialNames {
  /** coordinate order y, x = latitude (DCX_SPATIAL_Y), longitude (DCX_SPATIAL_X) */
  val DEGREES_SRS_NAME = "http://www.opengis.net/def/crs/EPSG/0/4326"

  /** coordinate order x, y = longitude (DCX_SPATIAL_X), latitude (DCX_SPATIAL_Y) */
  val RD_SRS_NAME = "http://www.opengis.net/def/crs/EPSG/0/28992"
}

trait SchemedSpatial {
  val scheme: Option[String]
  val value: Option[String]

  lazy val srsName: String = {
    scheme match {
      case Some("degrees") => SpatialNames.DEGREES_SRS_NAME
      case Some("RD") => SpatialNames.RD_SRS_NAME
      case Some(s) if s.trim.nonEmpty => s
      case _ => null // will suppress the XML attribute
    }
  }

  def determineSrsName(seq: Seq[String]): String = srsName match {
    case null =>
      if (seq.map(p => p.toDouble).max > 289000)
        SpatialNames.RD_SRS_NAME
      else
        SpatialNames.DEGREES_SRS_NAME
    case s => s
  }
}

case class SpatialPoint(scheme: Option[String],
                        x: Option[String],
                        y: Option[String],
                       ) extends SchemedSpatial with DebugEnhancedLogging {
  private lazy val sx: String = x.getOrElse("0")
  private lazy val sy: String = y.getOrElse("0")
  lazy val pos: String = srsName match {
    case SpatialNames.RD_SRS_NAME =>
      Try {
        val x = sx.toDouble
        val y = sy.toDouble
        if (x > y) s"$sy $sx"
        else s"$sx $sy"
      }.getOrElse {
        logger.warn(s"not numerical RD SpatialPoint: x=$sx, y=$sy")
        // schema validation will complain about just one of the values
        s"$sy $sx"
      }
    case SpatialNames.DEGREES_SRS_NAME => s"$sy $sx"
    case _ => s"$sy $sx"
  }

  override lazy val value: Option[String] = {
    (x ++ y).headOption
      .map(_ => pos)
  }

  lazy val dcxGml: Option[Elem] = value.map(value =>
    <dcx-gml:spatial srsName={ determineSrsName(Seq(sx, sy)) }>
      <Point xmlns="http://www.opengis.net/gml">
        <pos>{ value }</pos>
      </Point>
    </dcx-gml:spatial>
  )
}

case class SpatialBox(scheme: Option[String],
                      north: Option[String],
                      east: Option[String],
                      south: Option[String],
                      west: Option[String],
                     ) extends SchemedSpatial {
  private lazy val sWest: String = west.getOrElse("0")
  private lazy val sSouth: String = south.getOrElse("0")
  private lazy val sNorth: String = north.getOrElse("0")
  private lazy val sEast: String = east.getOrElse("0")
  private lazy val xy = (s"$sWest $sSouth", s"$sEast $sNorth")
  private lazy val yx = (s"$sSouth $sWest", s"$sNorth $sEast")
  /*
   * Note that Y is along North - South and X is along East - West
   * The lower corner is with the minimal coordinate values and upper corner with the maximal coordinate values
   * If x was increasing from West to East and y was increasing from South to North we would have
   * lower corner (x,y) = (West,South) and upper corner (x,y) = (East,North)
   * as shown in the schematic drawing of the box below.
   * This is the case for the WGS84 and RD coordinate systems, but not per se for any other system!
   *
   *                upper(x,y)=(E,N)
   *       *---N---u
   *       |       |
   *       W       E
   *  ^    |       |
   *  |    l---S---*
   *  |  lower(x,y)=(W,S)
   *  y
   *   x -->
   *
   */
  lazy val (lower: String, upper: String) = determineSrsName(Seq(sSouth,sNorth,sWest,sEast))match {
    case SpatialNames.RD_SRS_NAME => xy
    case SpatialNames.DEGREES_SRS_NAME => yx
    case _ => yx
  }

  override lazy val value: Option[String] = {
    (north ++ east ++ south ++ west).headOption
      .map(_ => s"($lower) ($upper)")
  }

  lazy val dcxGml: Option[Elem] = value.map(_ =>
    <dcx-gml:spatial>
      <boundedBy xmlns="http://www.opengis.net/gml">
          <Envelope srsName={ determineSrsName(Seq(sNorth, sSouth)) }>
              <lowerCorner>{ lower }</lowerCorner>
              <upperCorner>{ upper }</upperCorner>
          </Envelope>
      </boundedBy>
    </dcx-gml:spatial>
  )

}
