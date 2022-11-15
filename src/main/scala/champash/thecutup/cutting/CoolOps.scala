package champash.thecutup.cutting

import champash.thecutup.cutting.CuttingMachine._

import scala.annotation.tailrec

object CoolOps {

  implicit def ListToExtendedList[T](list: List[T]): ExtendedList[T] = new ExtendedList(list)

  class ExtendedList[T](list: List[T]) {

    def maybePadTo(h: Int, t: T, compressionMode: CompressionMode): List[T] = compressionMode match {
      case CompressVertically | CompressAll => list
      case _ => list.padTo(h, t)
    }
  }

  implicit def ListOfListsToExtendedListOfLists[T](ts: List[List[T]]): ExtendedListOfLists[T] = new ExtendedListOfLists(ts)

  class ExtendedListOfLists[T](ts: List[List[T]]) {
    def interleave: List[T] = {
      @tailrec
      def loop(remaining: List[List[T]], acc: List[T]): List[T] = remaining match {
        case Nil => acc.reverse
        case Nil :: tail => loop(tail, acc)
        case (t :: ts) :: tail => loop(tail :+ ts, t :: acc)
      }

      loop(ts, Nil)
    }
  }

  implicit def StringToExtendedString(string: String): ExtendedString = new ExtendedString(string)

  class ExtendedString(string: String) {
    def maybePad(w: Int, compressionMode: CompressionMode): String = compressionMode match {
      case Uncompressed | CompressVertically => string
      case _ => string.padTo(w, ' ')
    }

    def detab(t: Int): String =
      string.foldLeft(("", 0): (String, Int)) {
        case ((s, i), c) if c == '\t' =>
          val n = t - (i % t)
          val spaces = " " * n
          (s + spaces, i + n)
        case ((s, _), c) if c == '\n' => (s + c, 0)
        case ((s, i), c) => (s + c, i + 1)
      }._1

    def breakIntoLines(w: Int, maybeGive: Option[Int]): List[String] = {
      @tailrec
      def loop(rem: String, acc: List[String]): List[String] = {
        val tillNaturalLinebreak = rem.take(w).indexOf('\n')
        val tillLastSpaceOrDash = Math.max(rem.take(w).lastIndexOf(' '), rem.take(w).lastIndexOf('-')) + 1
        val fallbackBreakpoint = w
        if (rem.isEmpty) acc.reverse
        else if (tillNaturalLinebreak >= 0) loop(rem.drop(tillNaturalLinebreak + 1), rem.take(tillNaturalLinebreak) :: acc)
        else if (tillLastSpaceOrDash > w - maybeGive.getOrElse(w)) loop(rem.drop(tillLastSpaceOrDash), rem.take(tillLastSpaceOrDash) :: acc)
        else loop(rem.drop(fallbackBreakpoint), rem.take(fallbackBreakpoint) :: acc)
      }

      loop(string, Nil)
    }
  }

  implicit def listOfPagesToPageList(list: List[Page]): PageList = new PageList(list)

  class PageList(list: List[Page]) {
    def slice(n: Int): List[Page] = list.flatMap(_.slice(n))

    def splice(n: Int): List[Page] = list.grouped(n).map(_.reduce(_.splice(_))).toList

    def dump(): Unit = println(list.map(_.text.mkString("\n")).mkString("\n"))
  }

}
