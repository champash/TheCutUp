package champash.thecutup

import scala.annotation.tailrec

case class Page(text: List[String], w: Int, h: Int) {

  require(w >= 0, "Width less than 0")
  require(h >= 0, "Height less than 0")
  require(text.map(_.length).max <= w, "Text too wide")
  require(text.length <= h, "Text too tall")

  def splice(other: Page): Page =
    Page(
      text.zipAll(other.text, "", "")
        .map(t => t._1 + t._2),
      w + other.w, Math.max(h, other.h))

  def take(amount: Int): Page = {
    require(amount <= w)
    Page(text.map(_.take(amount)), amount, h)
  }

  def drop(amount: Int): Page = {
    require(amount <= w)
    Page(text.map(_.drop(amount)), w - amount, h)
  }

  def slice(n: Int): List[Page] = {
    @tailrec
    def loop(cutsStillToMake: Int, pageLeft: Page, acc: List[Page]): List[Page] = {
      if (cutsStillToMake == 0) acc.reverse
      else {
        val cutSize = Math.ceil(pageLeft.w.toDouble / cutsStillToMake).toInt
        loop(cutsStillToMake - 1, pageLeft.drop(cutSize), pageLeft.take(cutSize) :: acc)
      }
    }


    loop(n, pageLeft = this, acc = Nil)
  }

  override def toString: String = text.mkString("\n")
}
