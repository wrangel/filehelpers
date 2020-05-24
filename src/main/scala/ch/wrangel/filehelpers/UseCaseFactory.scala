package ch.wrangel.filehelpers

import java.nio.file.{Files, Path, Paths}


/** Protective object around use case singletons
 * https://alvinalexander.com/scala/factory-pattern-in-scala-design-patterns/
 */
object UseCaseFactory {

  /** Adds a common suffix to files in a directory */
  private class RenameFilesWithCommonPrefix(
                                             val directory: String,
                                             override val additionalElement: Option[String]
                                           ) extends UseCase {

    /** Runs the task */
    def run(): Unit = {
      files.foreach {
        filePath: Path =>
          val parent: String = filePath.getParent
            .toString
          val fileName: String = filePath.getFileName
            .toString
          val newPath: Path = Paths.get(parent, additionalElement.get + Constants.PartitionString + fileName)
          println(s"Renaming $filePath to $newPath")
          Files.move(filePath, newPath)
      }
    }

  }

  /** Removes the Conflict-Suffix introduced by DiskStation */
  private class RenameDiskStationConflictBackToOriginal(val directory: String) extends UseCase {

    /** Runs the task */
    def run(): Unit = {
      files.filter {
        filePath: Path =>



          Constants.ConflictSuffixElements
            .map {
              element: String =>
                filePath.getFileName
                  .toString
                  .contains(element)
            }
            .forall(_ equals true)
      }
        .foreach {
          filePath: Path =>
            val Seq(filePathWithoutExtension, extension) = FileUtilities.splitExtension(filePath, isPathNeeded = true)
            val newPath: Path = Paths.get(
              filePathWithoutExtension.substring(
                0,
                filePathWithoutExtension.indexOf(Constants.ConflictSuffixElements.head)
              )
                + extension
            )
            println(s"Renaming $filePath to $newPath")
            Files.move(filePath, newPath)
        }
    }

  }

  private class DeleteSubstringFromFileName(
                                             val directory: String,
                                             override val additionalElement: Option[String]
                                           ) extends UseCase {

    /** Runs the task */
    def run(): Unit = {
      files.foreach {
        filePath: Path =>
          val newPath: Path = Paths.get(
            filePath.toString
            .replace(additionalElement.get, "")
          )
          println(s"Renaming $filePath to $newPath")
          Files.move(filePath, newPath)
      }
    }

  }

  private class DeleteBeforeLastSubstring(
                                       val directory: String,
                                       override val additionalElement: Option[String]
                                     ) extends UseCase {

    /** Runs the task */
    def run(): Unit = {
      files.foreach {
        filePath: Path =>
          val element: String = additionalElement.get
          val newPath: Path =
            if(filePath.toString.contains(element)) {
              val Seq(name: String, extension: String) = FileUtilities.splitExtension(filePath, isPathNeeded = false)
              Paths.get(
                filePath.getParent
                  .toString,
                name.substring(name.lastIndexOf(element) + element.length, name.length) + extension
              )
            }
            else
              filePath
          println(s"Renaming $filePath to $newPath")
          Files.move(filePath, newPath)
      }
    }

  }

  ////
  private class FindDuplicateFiles(val directory: String) extends UseCase {

    /** Runs the task */
    def run(): Unit = {
      files.foreach {
        filePath: Path =>
      }
    }

  }

  /** Factory method
   *
   * @param useCase Applicable use case
   * @return Use case singleton
   */
  def apply(
             directory: String,
             useCase: String,
             additionalElement: Option[String],
           ): UseCase = {
    useCase match {
      case "prefix" =>
        new RenameFilesWithCommonPrefix(directory, additionalElement)
      case "conflict" =>
        new RenameDiskStationConflictBackToOriginal(directory)
      case "deleteElement" =>
        new DeleteSubstringFromFileName(directory, additionalElement)
      case "deleteBeforeLastOccurrence" =>
        new DeleteBeforeLastSubstring(directory, additionalElement)
    }
  }

}
