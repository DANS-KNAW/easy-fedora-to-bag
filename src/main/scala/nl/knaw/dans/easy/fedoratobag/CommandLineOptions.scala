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

import java.nio.file.Paths
import better.files.File
import nl.knaw.dans.easy.fedoratobag.OutputFormat.OutputFormat
import nl.knaw.dans.easy.fedoratobag.TransformationType.{ FEDORA_VERSIONED, ORIGINAL_VERSIONED, TransformationType }
import org.rogach.scallop.{ ScallopConf, ScallopOption, ValueConverter, singleArgConverter }

import scala.xml.Properties

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-fedora-to-bag"
  version(configuration.version)
  val description: String = s"""Tool for exporting datasets from Fedora and constructing Archival/Submission Information Packages."""
  val synopsis: String =
    s"""
       |  easy-fedora-to-bag {-d <dataset-id> | -i <dataset-ids-file>} [--skip-list <skip-dataset-ids-file>] -o <output-dir> [-s] [-l <log-file>] [-e | -p] -f { AIP | SIP } <transformation>
     """.stripMargin

  version(s"$printedName v${ configuration.version }")
  banner(
    s"""
       |  $description
       |
       |Usage:
       |
       |$synopsis
       |
       |Options:
       |""".stripMargin)

  implicit val transformationTypeConverter: ValueConverter[TransformationType] = singleArgConverter(TransformationType.withName)
  implicit val outputFormatConverter: ValueConverter[OutputFormat] = singleArgConverter(OutputFormat.withName)
  implicit val fileConverter: ValueConverter[File] = singleArgConverter(File(_))

  val datasetId: ScallopOption[DatasetId] = opt(name = "datasetId", short = 'd',
    descr = "A single easy-dataset-id to be transformed. Use either this or the input-file argument")
  val inputFile: ScallopOption[File] = opt(name = "input-file", short = 'i',
    descr = "File containing a newline-separated list of easy-dataset-ids to be transformed. Use either this or the dataset-id argument")
  val skipDatasets: ScallopOption[File] = opt(name = "skip-list",
    descr = s"File containing a newline-separated list of easy-dataset-ids to be skipped")
  val outputDir: ScallopOption[File] = opt(name = "output-dir", short = 'o',
    descr = "Empty directory that will be created if it doesn't exist. Successful bags (or packages) will be moved to this directory.")
  val outputFormat: ScallopOption[OutputFormat] = opt(name = "output-format", short = 'f',
    descr = OutputFormat.values.mkString("Output format: ", ", ", ". 'SIP' is only implemented for simple, it creates the bags one directory level deeper. easy-bag-to-deposit completes these sips with deposit.properties"))
  val logFile: ScallopOption[File] = opt(name = "log-file", short = 'l',
    descr = s"The name of the logfile in csv format. If not provided a file $printedName-<timestamp>.csv will be created in the home-dir of the user.",
    default = Some(Paths.get(Properties.userHome).resolve(s"$printedName-$now.csv")))
  val strictMode: ScallopOption[Boolean] = opt(name = "strict", short = 's',
    descr = "If provided, the transformation will check whether the datasets adhere to the requirements of the chosen transformation.")
  val europeana: ScallopOption[Boolean] = opt(name = "europeana", short = 'e',
    descr = "If provided, only the largest pdf/image will selected as payload.")
  val noPayload: ScallopOption[Boolean] = opt(name = "no-payload", short = 'p',
    descr = "If provided, no payload files will be exported, i.e. only the metadata is present in the bag.")
  val transformation: ScallopOption[TransformationType] = trailArg(name = "transformation", required = true,
    descr = TransformationType.values.mkString("The type of transformation used: ", ", ", "."))

  conflicts(datasetId, List(inputFile))
  conflicts(noPayload, List(europeana))
  validateOpt(inputFile) {
    case Some(f) if !f.toJava.isFile => Left(s"$f does not exist or is not a file")
    case _ => Right(())
  }
  validateOpt(logFile) {
    case Some(f) if f.exists => Left(s"$f should not exist")
    case _ => Right(())
  }
  codependent(outputFormat, outputDir)
  validateOpt(transformation, outputDir) {
    case (None, _) => Left(s"trailing argument 'transformation' is mandatory") // required so won't happen
    case (Some(FEDORA_VERSIONED), _) if inputFile.isEmpty => Left(s"argument 'input-file' is mandatory for $FEDORA_VERSIONED")
    case (Some(FEDORA_VERSIONED), _) => Right(())
    case (Some(ORIGINAL_VERSIONED), _) if noPayload() => Left(s"no-payload conflicts with original-versioned")
    case (Some(t), None) => Left(s"argument 'output-dir' is mandatory for $t")
    case (_, Some(dir)) =>
      if (dir.exists) {
        if (!dir.isDirectory) Left(s"output-dir $dir does not reference a directory")
        else if (dir.nonEmpty) Left(s"output-dir $dir exists but is not an empty directory")
             else if (!dir.isWriteable) Left(s"output-dir $dir exists and is empty but is not writeable by the current user")
                  else Right(())
      }
      else {
        dir.createDirectories()
        Right(())
      }
  }

  footer("")
}
