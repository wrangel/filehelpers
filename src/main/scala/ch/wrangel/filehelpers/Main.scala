package ch.wrangel.filehelpers

import java.nio.file.{Files, Paths}

import scala.util.{Success, Try}

object Main extends App {

  // 1981, 1984
  val dir = "/Volumes/photo/1997"
  FileUtilities.iterateFiles(dir)
    .map(_.getFileName.toString)
    .filter(c => c.contains(".tif") | c.contains(".png") | c.contains(".jpg"))
    .map {
      s: String =>
        val end1 = s.lastIndexOf(".")
        val start1 = s.lastIndexOf("__")
        val end2 = s.indexOf("__")
        val start2 = s.indexOf("_")
        (s, s.substring(start1 + 2, end1).toInt, s.substring(0, 4), s.substring(start2 + 1, end2))
    }
    .sortBy(_._2)
    .map {
      s =>
        val tmp = ((s._3.toInt - 1978) * 100 + s._2).toString
        val newTime = "0" * (6 - tmp.length) + tmp
        Files.move(Paths.get(dir, s._1), Paths.get(dir, s._1.replace(s._4, newTime)))
    }
  System.exit(0)
  ////
  
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