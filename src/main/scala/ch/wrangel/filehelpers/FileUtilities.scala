package ch.wrangel.filehelpers

import java.nio.file._

import scala.jdk.StreamConverters._

/*Utilities for file manipulation */
object FileUtilities {

  /** Iterates through a directory [[String]]
   *
   * @param directory             [[String]] representation of directory path
   * @param walk                  [[Boolean]] indicating whether the iteration will be recursive
   * @param admissibleFileFormats Optional [[Seq]] of relevant file formats
   * @return [[Seq]] of file [[Path]]s within the directory
   */
  def iterateFiles(
                    directory: String,
                    walk: Boolean = false,
                    admissibleFileFormats: Option[Seq[String]] = None
                  ): Seq[Path] = {
    (
      if (walk) {
        Files.walk(Paths.get(directory))
      } else
        Files.list(Paths.get(directory))
      )
      .filter(Files.isRegularFile(_))
      .filter {
        filePath: Path =>
          !Files.isHidden(filePath) &
            !filePath.toString.contains("@") & {
            admissibleFileFormats match {
              case Some(fileExtensions: Seq[String]) =>
                fileExtensions.exists {
                  fileExtension: String =>
                    FileSystems.getDefault
                      .getPathMatcher(s"glob:**.$fileExtension")
                      .matches(filePath)
                }
              case None =>
                true
            }
          }
      }
      .toScala(Seq)
  }

  /** Splits a filename into body and extension
   *
   * @param filePath [[Path]] to the file whose trailer is to be removed
   * @return [[String]] with removed trailers
   */
  def splitExtension(filePath: Path, isPathNeeded: Boolean): Seq[String] = {
    val relevantPathPortion: String = if (isPathNeeded)
      filePath.toString
    else
      filePath.getFileName.toString
    val dotPosition: Int = relevantPathPortion.reverse.indexOf(".") + 1
    val length: Int = relevantPathPortion.length
    Seq(
      relevantPathPortion.substring(0, length - dotPosition),
      relevantPathPortion.substring(length - dotPosition, length)
    )
  }

}