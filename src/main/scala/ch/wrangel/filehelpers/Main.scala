package ch.wrangel.filehelpers

import scala.util.{Success, Try}

object Main extends App {
  
    // directory, use case, optional additional element (suffix or removableElement)
  UseCaseFactory(
    args.head,
    args(1),
    Try { args(2) }
     match {
      case Success (arg: String) =>
        Some(arg)
      case _ =>
        None
    }
  ).run()

}