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
package nl.knaw.dans.easy.fedora2vault

import java.nio.file.{ Path, Paths }

import better.files.File
import nl.knaw.dans.easy.fedora2vault.TransformationType.TransformationType
import org.rogach.scallop.{ ScallopConf, ScallopOption, ValueConverter, singleArgConverter }

import scala.xml.Properties

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-fedora2vault"
  version(configuration.version)
  val description: String = s"""Tool for exporting datasets from Fedora and constructing AIP-bags to be stored in the bag stores"""
  val synopsis: String =
    s"""
       |  easy-fedora2vault {-d <dataset-id> | -i <dataset-ids-file>} [-o <staged-AIP-dir>] [-f <file-metadata>] [-u <depositor>] [-s] [-l <log-file>] <transformation>
       |  easy-fedora2vault -d <dataset-id> -o <staged-AIP-dir> <transformation>
       |  easy-fedora2vault -s -u <depositor> -i <dataset-ids-file> -o <staged-AIP-dir> -l <log-file> <transformation>""".stripMargin

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

  val datasetId: ScallopOption[DatasetId] = opt(name = "datasetId", short = 'd',
    descr = "A single easy-dataset-id to be transformed. Use either this or the input-file argument")
  private val inputPath: ScallopOption[Path] = opt(name = "input-file", short = 'i',
    descr = "File containing a newline-separated list of easy-dataset-ids to be transformed. Use either this or the dataset-id argument")
  val inputFile: ScallopOption[File] = inputPath.map(File(_))
  private val fileListPath: ScallopOption[Path] = opt(name = "file-list", short = 'f',
    descr = "A csv-file with metadata about extra files to be added to the AIP bag")
  val fileList: ScallopOption[File] = fileListPath.map(File(_))
  private val outputDirPath: ScallopOption[Path] = opt(name = "output-dir", short = 'o',
    descr = "Empty directory in which to stage the created AIP bags. It will be created if it doesn't exist.")
  val outputDir: ScallopOption[File] = outputDirPath.map(File(_))
  val depositor: ScallopOption[Depositor] = opt(name = "depositor", short = 'u',
    descr = "The depositor for these datasets. If provided, only datasets from this depositor are transformed.")
  private val logFilePath: ScallopOption[Path] = opt(name = "log-file", short = 'l',
    descr = "The name of the logfile in csv format. If not provided a file easy-fedora2vault-<timestamp>.csv will be created in the home-dir of the user.",
    default = Some(Paths.get(Properties.userHome).resolve(s"easy-fedora2vault-${ now.replaceAll("[:.]", "-") }.csv")))
  val logFile: ScallopOption[File] = logFilePath.map(File(_))
  val strictMode: ScallopOption[Boolean] = opt(name = "strict", short = 's',
    descr = "If provided, the transformation will check whether the datasets adhere to the requirements of the chosen transformation.")
  val transformation: ScallopOption[TransformationType] = trailArg(name = "transformation",
    descr = "The type of transformation used. Only 'simple' is implemented yet.")

  requireOne(datasetId, inputPath)

  validatePathExists(inputPath)
  validatePathIsFile(inputPath)

  validatePathExists(fileListPath)
  validatePathIsFile(fileListPath)

  validate(outputDir)(dir => {
    if (dir.exists) {
      if (!dir.isDirectory) Left(s"outputDir $dir does not reference a directory")
      else if (dir.nonEmpty) Left(s"outputDir $dir exists but is not an empty directory")
           else if (!dir.isWriteable) Left(s"outputDir $dir exists and is empty but is not writeable by the current user")
                else Right(())
    }
    else Right(())
  })

  footer("")
}
