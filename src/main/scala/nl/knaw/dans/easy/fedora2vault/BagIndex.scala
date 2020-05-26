package nl.knaw.dans.easy.fedora2vault

import java.io.IOException
import java.net.URI

import scalaj.http.Http

import scala.util.{ Failure, Try }

case class BagIndex(bagIndexUri: URI) {

  /** An Exception that is fatal for the batch of datasets */
  case class BagIndexException(msg: String, cause: Throwable) extends IOException(msg, cause)

  private val url: URI = bagIndexUri.resolve("search")
  def bagByDoi(doi: String): Try[Option[String]] = Try {
    Http(url.toString)
      .param("doi", doi)
      .header("Accept", "text/xml")
      .asString
  }.recoverWith {
    case t: Throwable => Failure(BagIndexException(t.getMessage, t))
  }.map {
    case response if response.code == 400 => None
    case response if response.code == 200 => Some(response.body)
    case response => throw BagIndexException(s"Not expected response code from bag-index. url='${ url } }', doi='$doi', response: ${ response.code } - ${ response.body }",null)
  }
}
