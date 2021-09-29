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

import javax.naming.directory.{ Attributes, SearchControls }
import javax.naming.ldap.LdapContext

import scala.collection.JavaConverters._
import scala.util.Try

case class LdapUser(id: String, name: String, email: String)

class Ldap(ctx: LdapContext) {
  // variant of https://github.com/DANS-KNAW/easy-deposit-agreement-creator/blob/fd57c48d7aea219577b06b38e9a96527cacd3a68/src/main/scala/nl/knaw/dans/easy/agreement/datafetch/Ldap.scala#L36
  def query(depositorId: String): Try[LdapUser] = Try {
    val searchFilter = s"(&(objectClass=easyUser)(uid=$depositorId))"
    val searchControls = new SearchControls() {
      setSearchScope(SearchControls.SUBTREE_SCOPE)
    }

    ctx.search("dc=dans,dc=knaw,dc=nl", searchFilter, searchControls)
      .asScala
      .toStream
      .map(_.getAttributes)
      .headOption
      .getOrElse { throw new Exception(s"Could not find $depositorId") }
  }.map(implicit attrs => {
    val name = getOrEmpty("displayname")
    val mail = getOrEmpty("mail")
    LdapUser(depositorId, name, mail)
  })

  private def getOrEmpty(attrID: String)(implicit attrs: Attributes) = {
    Option(attrs get attrID).fold("")(_.get.toString)
  }
}
