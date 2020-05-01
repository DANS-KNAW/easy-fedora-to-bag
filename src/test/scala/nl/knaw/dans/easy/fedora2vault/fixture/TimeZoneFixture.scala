package nl.knaw.dans.easy.fedora2vault.fixture

import java.util.TimeZone

trait TimeZoneFixture {
  // for proper conversion of DateTime to date string by EMD's IsoDate/BasicDate
  TimeZone.setDefault(TimeZone.getTimeZone("Europe/Amsterdam"))
}
