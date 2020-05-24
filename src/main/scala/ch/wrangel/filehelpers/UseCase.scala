package ch.wrangel.filehelpers

import java.nio.file.Path

trait UseCase {

  val directory: String
  val additionalElement: Option[String] = None
  val files: Seq[Path] = FileUtilities.iterateFiles(directory)

  /** Runs the task */
  def run(): Unit

}