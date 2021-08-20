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

import java.net.{ URI, URL }

import better.files.File
import better.files.File.root
import com.yourmediashelf.fedora.client.FedoraCredentials

import javax.naming.Context
import org.apache.commons.configuration.PropertiesConfiguration

case class Configuration(version: String,
                         fedoraCredentials: FedoraCredentials,
                         databaseConnection: DatabaseConnection,
                         ldapEnv: LdapEnv,
                         bagIndexUrl: URI,
                         stagingDir: File,
                         abrMapping: AbrMappings,
                        )

object Configuration {

  def apply(home: File): Configuration = {
    val cfgPath = Seq(
      root / "etc" / "opt" / "dans.knaw.nl" / "easy-fedora-to-bag",
      home / "cfg")
      .find(_.exists)
      .getOrElse { throw new IllegalStateException("No configuration directory found") }
    val properties = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load((cfgPath / "application.properties").toJava)
    }

    new Configuration(
      version = (home / "bin" / "version").contentAsString.stripLineEnd,
      fedoraCredentials = new FedoraCredentials(
        new URL(properties.getString("fcrepo.url")),
        properties.getString("fcrepo.user"),
        properties.getString("fcrepo.password"),
      ),
      DatabaseConnection (
        properties.getString("fsrdb.db-connection-url"),
        properties.getString("fsrdb.db-connection-username"),
        properties.getString("fsrdb.db-connection-password")
      ),
      new LdapEnv {
        put(Context.PROVIDER_URL, properties.getString("auth.ldap.url"))
        put(Context.SECURITY_AUTHENTICATION, "simple")
        put(Context.SECURITY_PRINCIPAL, properties.getString("auth.ldap.user"))
        put(Context.SECURITY_CREDENTIALS, properties.getString("auth.ldap.password"))
        put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
      },
      new URI(properties.getString("bag-index.url")),
      File(properties.getString("staging.dir")),
      AbrMappings(cfgPath / "EMD_acdm.xsl"),
    )
  }
}
