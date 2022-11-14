package champash.thecutup

import org.scalatest.{Matchers, WordSpec}
import CoolOps._

class CoolOpsSpec extends WordSpec with Matchers {
  "interleave" should {
    "work" in {
      val lol = List(List(1, 4, 7, 9), List(2, 5), List(3, 6, 8))
      lol.interleave shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }
  "detab" should {
    "work" in {
      val t = 8
      val s = "\tblah\tblah blah \n\tblah blah blah"
      s.detab(t) shouldBe "        blah    blah blah \n        blah blah blah"
    }
  }

  "breakIntoPages" should {
    "work" in {
      val s =
        """This is a line.
          |This is a longer line which should be broken up a couple times.
          |This line has a reallyreallyreallyreally long word.
          |Longerlongerlongerlongerlongerlongerlongerlongerlonger.
          |This long-long-long-long-long line has convenient dashes.
          |""".stripMargin
      val e = "This is a line.\nThis is a \nlonger line \nwhich should be "+
        "\nbroken up a \ncouple times.\nThis line has a \nreallyreallyrea-\nllyreally " +
        "long \nword.\nLongerlongerlon-\ngerlongerlonger-\nlongerlongerlon-\ngerlonger.\n" +
        "This long-long-\nlong-long-long \nline has \nconvenient \ndashes."
     s.breakIntoLines(16, 8).mkString("\n") shouldBe e
    }
  }
}
