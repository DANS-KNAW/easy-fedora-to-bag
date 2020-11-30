package nl.knaw.dans.easy.fedoratobag

import scalaj.http.Http

import scala.util.{ Success, Try }

object Resolver {

  def getDatasetId(id: String): Try[String] = {
    id.slice(0, 3) match {
      case "eas" => Success(id)
      case "urn" => resolve(s"http://www.persistent-identifier.nl/?identifier=$id")
      case "10." => resolve(s"https://doi.org/$id")
    }
  }

  private def resolve(url: String) = {
    Try(Http(url).asString).map {
      case response if response.code == 302 =>
        response
          .header("Location")
          .map(_.replaceAll(".*/","").replace("%3A",":"))
          .getOrElse(throw new Exception(s"no location header returned by $url - ${ response.body }"))
      case response =>
        throw new Exception(s"Not expected response code from '$url' ${ response.code } - ${ response.body }", null)
    }
  }
}
