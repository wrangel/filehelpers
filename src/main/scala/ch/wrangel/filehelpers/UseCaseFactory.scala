package ch.wrangel.filehelpers

import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.sys.process.Process
import scala.util.{Failure, Success, Try}


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
            val newPath: Path = FileUtilities.getNewPath(filePath)
            println(s"Renaming $filePath to $newPath")
            Files.move(filePath, newPath)
        }
    }
  }

  /** Finds cluster of duplicate files after Diskstation synchronization issues resulting in "conflict" files.
   * Gets the files with correct datetimeoriginal timestamp (according to filename) and renames one of them, if
   * the size of the file is the largest in the cluster
   */
  private class FindDuplicateFiles(val directory: String) extends UseCase {
    /** Runs the task */
    def run(): Unit = {
      // Per file stem, get duplicate file paths, file sizes, file timestamps, and exif timestamps
      val basics: Seq[(Path, Path, Long, Long, Long)] = files.map {
        filePath: Path =>
          println(s"Looking at $filePath")
          // refactored method start
          val newPath: Path = FileUtilities.getNewPath(filePath)
          // refactored method end
          (
            newPath,
            filePath,
            Files.size(filePath),
            {
              val newPathString: String = newPath.toString
              StringUtilities.convertStringToLong(
                newPathString.substring(
                  0, Seq(
                    newPathString.lastIndexOf("."),
                    {
                      val doubleUnderscore: Int = newPathString.indexOf("__")
                      if(doubleUnderscore == -1)
                        10000
                      else doubleUnderscore
                    }
                  )
                    .min
                )
              )
            },
            {
              val p = Process(s"""exiftool -DateTimeOriginal "$filePath"""".trim).!!
              Try {
                StringUtilities.convertStringToLong(p.substring(p.indexOf(":") + 1, p.length))
              }
                .getOrElse(1L)
            }
          )
      }
      // Count duplicates per file stem
      val occurrences: Map[Path, Int] = basics.groupBy(_._1)
        .view
        .mapValues(_.size)
        .toMap
      // Collect duplicates and their characteristics
      val overview: MMap[Path, ListBuffer[(Path, Long, Long, Long)]] = MMap[Path, ListBuffer[(Path, Long, Long, Long)]]()
      basics.foreach {
        case(newPath: Path, filePath: Path, size: Long, fileTimestamp: Long, exifTimestamp: Long) =>
          // Get only duplicated files
          if(occurrences(newPath) > 1) {
            if(overview.contains(newPath))
              overview(newPath) += ((filePath, size, fileTimestamp, exifTimestamp))
            else
              overview.addOne(newPath -> ListBuffer((filePath, size, fileTimestamp, exifTimestamp)))
          }
      }
      // Triage files into one to be renamed, and all others to be deleted
      val triage: MMap[Path, Map[Boolean, ListBuffer[(Path, Long, Long, Long)]]] = overview.map {
        case (newPath: Path, characteristics: ListBuffer[(Path, Long, Long, Long)]) =>
          newPath ->
            characteristics.groupBy {
              element: (Path, Long, Long, Long) =>
                // Get the ones where file timestamp and exif timestamp concur
                element._3 == element._4
            }
      }
      // Rename or delete files
      triage.foreach {
        element: (Path, Map[Boolean, ListBuffer[(Path, Long, Long, Long)]]) =>
          val triaged: Map[Boolean, (Long, Long)] = element._2.map {
            element2: (Boolean, ListBuffer[(Path, Long, Long, Long)]) =>
              element2._1 -> (
                element2._2.map(_._2).min,
                element2._2.map(_._2).max
              )
          }
          Try {
            triaged(true)
          } match {
            case Success (ok: (Long, Long)) =>
              Try {
                triaged(false)
              } match {
                // 1) both false and true are available
                case Success (del: (Long, Long)) =>
                  // treat only if correctly dated files are larger than incorrectly dated files
                  if(ok._1 >= del._2) {
                    val renamingCandidates = element._2(true)
                    FileUtilities.deleteFiles(element._2(false).map(_._1) ++ renamingCandidates.tail.map(_._1))
                    FileUtilities.renameFile(renamingCandidates.head._1, element._1)
                  }
                // 2) only true is available
                case Failure (_) =>
                  val renamingCandidates = element._2(true)
                  FileUtilities.deleteFiles(renamingCandidates.tail.map(_._1))
                  FileUtilities.renameFile(renamingCandidates.head._1, element._1)
              }
            case Failure (_) =>
              Try {
                triaged(false)
              } match {
                // only false is available
                case Success (del: (Long, Long)) =>
                  // Keep one file
                  FileUtilities.deleteFiles(element._2(false).map(_._1).tail)
                // none is available (not possible)
                case Failure (_) =>
              }
          }

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

  private class DeleteDuplicateSubstring(
                                           val directory: String,
                                           override val additionalElement: Option[String]
                                         ) extends UseCase {
    val element: String = additionalElement.get
    /** Runs the task */
    override def run(): Unit = {
      FileUtilities.iterateFiles(directory)
        .map(c => (c, c.getParent.toString, c.getFileName.toString))
        .map(c => Seq(c._1, Paths.get(c._2, c._3.split(element).toSet.mkString(element))))
        .foreach {
          c =>
            println(s"Renaming ${c.head} to ${c.last}")
            Try {
              Files.move(c.head, c.last)
            }
              .getOrElse(Files.move(c.head, Paths.get(c.last + "____DUPLICATE")))
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
      case "conflictCluster" =>
        new FindDuplicateFiles(directory)
      case "deleteElement" =>
        new DeleteSubstringFromFileName(directory, additionalElement)
      case "deleteBeforeLastOccurrence" =>
        new DeleteBeforeLastSubstring(directory, additionalElement)
      case "deleteDuplicateElement" =>
        new DeleteDuplicateSubstring(directory, additionalElement)

    }
  }

}