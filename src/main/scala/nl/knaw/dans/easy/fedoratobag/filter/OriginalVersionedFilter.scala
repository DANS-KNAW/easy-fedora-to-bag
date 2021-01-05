package nl.knaw.dans.easy.fedoratobag.filter

case class OriginalVersionedFilter() extends SimpleDatasetFilter {
  override val allowOriginalAndOthers: Boolean = true
}
