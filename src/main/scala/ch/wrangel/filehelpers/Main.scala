package ch.wrangel.filehelpers

import scala.util.{Success, Try}

object Main extends App {

  Try {
    UseCaseFactory(
      args.head,
      args(1),
      Try { args(2) } match {
        case Success(arg: String) =>
          Some(arg)
        case _ =>
          None
      }
    ).run()
  }.getOrElse(
      System.out.println(
        """ Welcome to the file helper.
          | Choose one of the following options:
          | a) <directory> prefix <prefixDelimiter>: Add a prefix to the filename, delimited by prefixDelimiter
          | b) <directory> conflict: Purges a directory from DiskStation "conflict" files
          | c) <directory> duplicateFiles: Finds duplicate files in a directory
          | d) <directory> deleteSubstring <substringToDelete>: Deletes <substringToDelete> from filename
          | e) <directory> deleteBeforeLastOccurrence <substringToConsider>: Deletes everything before  <substringToConsider>
          | f) <directory> deleteDuplicateSubstring <substringToConsider>: Deletes specific duplicate <substringToConsider>
          | g) <directory> moveToParentDir: Moves all nested files to <directory>
          |""".stripMargin)
    )
}
