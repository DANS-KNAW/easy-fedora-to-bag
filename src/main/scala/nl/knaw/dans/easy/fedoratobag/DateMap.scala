package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.lib.string._
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.types.EmdConstants.DateScheme
import nl.knaw.dans.pf.language.emd.types.{ BasicDate, IsoDate }

import scala.collection.JavaConverters._
import scala.xml.{ Elem, Text }

object DateMap {
  def isOtherDate(kv: (String, Iterable[Elem])): Boolean = !Seq("dct:created", "dct:available").contains(kv._1)

  private def dateLabel(key: String): String = key.toOption.map("dct:" + _).getOrElse("dct:date")

  def apply(emd: EasyMetadataImpl): Map[DatasetId, Seq[Elem]] = {
    val basicDates = emd.getEmdDate.getAllBasicDates.asScala.mapValues(_.asScala.map(toXml))
    val isoDates = emd.getEmdDate.getAllIsoDates.asScala.mapValues(_.asScala.map(toXml))
    val stringToElems = (basicDates.toSeq ++ isoDates.toSeq)
      .groupBy(kv => dateLabel(kv._1))
      .mapValues(_.flatMap(_._2))

    val date = "([0-9]{4}-[0-9]+-[0-9]+|[0-9]{4}-[0-9]+|[0-9]{4})"
    val regexp = s"$date *(through|to|tot|-) *$date"

    val periods = stringToElems.values.flatten
      .withFilter(_.text.matches(regexp))
      .map(elem => elem.copy(
        child = new Text(elem.text.replaceAll(regexp, "$1/$3")),
      )).toSeq
    stringToElems.mapValues(_.map(elem => elem.copy(
      child = new Text(elem.text.replaceAll(regexp, "$3")),
    ))) + ("ddm:datesOfCollection" -> periods)
  }

  private def toXml(value: IsoDate): Elem = <label xsi:type={ orNull(value.getScheme) }>{ fixDate(value) }</label>

  private def toXml(value: BasicDate): Elem = <label xsi:type={ orNull(value.getScheme) }>{ value }</label>

  // meaning: or omit attribute
  private def orNull(dateScheme: DateScheme): String = Option(dateScheme).map("dct:" + _.toString).orNull

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
}
