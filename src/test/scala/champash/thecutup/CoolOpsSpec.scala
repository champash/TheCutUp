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
    "work when the tab comes first" in {
      val t = 8
      val s = "\tblah"
      s.detab(t) shouldBe "        blah"
    }

    "work when the tab comes in the middle" in {
      val t = 8
      val s = "blah\tblah"
      s.detab(t) shouldBe "blah    blah"
    }

    "work when the tab comes at the end" in {
      val t = 8
      val s = "blah\t"
      s.detab(t) shouldBe "blah    "
    }
  }

  "breakIntoPages" should {
    "work for natural breaks" in {
      val s = "This text has\ntwo lines."
      s.breakIntoLines(16, Some(8)) shouldBe List("This text has", "two lines.")
    }

    "work with dashes" in {
      val s = "123456789abcde-f"
      s.breakIntoLines(16, Some(8)) shouldBe List("123456789abcde-", "f")
    }

    "work with spaces" in {
      val s = "123456789 abcdef"
      s.breakIntoLines(16, Some(8)) shouldBe List("123456789 ", "abcdef")
    }

    "work with overlong words" in {
      val s = "Pneumonoultramicroscopicsilicovolcanoconiosis"
      s.breakIntoLines(16, Some(8)) shouldBe List("Pneumonoultramic", "roscopicsilicovo", "lcanoconiosis")
    }
  }
}
