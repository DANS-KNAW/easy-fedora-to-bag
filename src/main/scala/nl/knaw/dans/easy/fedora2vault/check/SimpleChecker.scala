package nl.knaw.dans.easy.fedora2vault.check

import nl.knaw.dans.easy.fedora2vault.BagIndex

case class SimpleChecker(override val bagIndex: BagIndex) extends TransformationChecker {
  override def forbiddenTitle(title: String): Boolean = {
    title.toLowerCase.contains("thematische collectie")
  }
}
