package champash.thecutup

import champash.thecutup.CuttingMachine.{CompressHorizontally, CompressVertically, StripedPage}

import CoolOps._

import scala.util.Random

object Demo {
  def main(args: Array[String]): Unit = {
    val text1 = "In mathematics, the factorial of a non-negative integer n, denoted by n!, is the product of all positive integers less than or equal to n. The factorial of n also equals the product of n with the next smaller factorial: For example, The value of 0! is 1, according to the convention for an empty product."
    val text2 = "William Seward Burroughs II was an American writer and visual artist, widely considered a primary figure of the Beat Generation and a major postmodern author who influenced popular culture and literature."
    val text3 = "The era that most people think of when they talk about \"cavemen\" is the Paleolithic Era, sometimes referred to as the Stone Age (though actually the Paleolithic is but one part of the Stone Age). This era extends from more than 2 million years into the past until sometime between 40,000 and 10,000 years ago."
    val w = 80
    val h = 5
    val g = 8
    val n = 5
    val t = 2
    val random = new Random()
    random.setSeed(23)
    CuttingMachine.cut(List(text1, text2, text3), w, h, t, g, n, StripedPage, CompressHorizontally, CompressVertically, Some(random)).dump()
  }
}
