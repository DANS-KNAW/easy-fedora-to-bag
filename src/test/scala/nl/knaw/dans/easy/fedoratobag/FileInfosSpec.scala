package nl.knaw.dans.easy.fedoratobag

import nl.knaw.dans.easy.fedoratobag.filter._
import nl.knaw.dans.easy.fedoratobag.fixture.TestSupportFixture

import java.nio.file.Paths
import scala.util.Success

class FileInfosSpec extends TestSupportFixture {
  private val fileInfo = new FileInfo("easy-file:1", Paths.get("y.txt"), "y.txt", size = 2, mimeType = "text/plain", accessibleTo = "ANONYMOUS", visibleTo = "ANONYMOUS", contentDigest = None, additionalMetadata = None)
  "selectForXxxBag" should "return files for two bags" in {
    val fileInfos = List(
      "original/x.txt",
      "y.txt",
    ).zipWithIndex.map { case (str, n) =>
      val path = Paths.get(str)
      new FileInfo(s"easy-file:$n", path, path.toFile.getName, size = 2, mimeType = "text/plain", accessibleTo = "ANONYMOUS", visibleTo = "ANONYMOUS", contentDigest = None, additionalMetadata = None)
    }
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = false)
    for2nd shouldBe fileInfos
    for1st shouldBe Success(fileInfos.slice(0, 1))
  }
  it should "return one file for each bag" in {
    val fileInfos = List(
      fileInfo.copy(fedoraFileId = "easy-file:1", path = Paths.get("original/y.txt"), accessibleTo = "NONE", visibleTo = "NONE"),
      fileInfo.copy(fedoraFileId = "easy-file:2"),
    )
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = false)
    for2nd shouldBe fileInfos.slice(1, 2)
    for1st shouldBe Success(fileInfos.slice(0, 1))
  }
  it should "return files for one bag" in {
    val fileInfos = List(
      "rabarbera/x.txt",
      "x.txt",
    ).zipWithIndex.map { case (str, n) =>
      val path = Paths.get(str)
      new FileInfo(s"easy-file:$n", path, path.toFile.getName, size = 2, mimeType = "text/plain", accessibleTo = "ANONYMOUS", visibleTo = "ANONYMOUS", contentDigest = None, additionalMetadata = None)
    }
    val for2nd = fileInfos.selectForSecondBag(isOriginalVersioned = true)
    val for1st = fileInfos.selectForFirstBag(<emd/>, for2nd.nonEmpty, europeana = false)
    for2nd shouldBe empty
    for1st shouldBe Success(fileInfos)
  }
}
