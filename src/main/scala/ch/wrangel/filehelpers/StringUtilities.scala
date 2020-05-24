package ch.wrangel.filehelpers


/*Utilities for String manipulation */
object StringUtilities {

  /** Converts a [[String]] to [[Long]]
   *
   * @param s [[String]] to be converted
   * @return Converted [[Long]]
   */
  def convertStringToLong(s: String): Long = {
    s.replaceAll("[^0-9]", "")
      .toLong
  }

}
