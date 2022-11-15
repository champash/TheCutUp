package champash.thecutup

import champash.thecutup.cutting.Page
import org.scalatest.{Matchers, WordSpec}

class PageSpec extends WordSpec with Matchers {

  "Page" should {

    val evenPage = Page(
      List("aaabbbcccddee",
        "aaabbbcccddee",
        "aaabbbcccddee",
        "aaabbbcccddee"), 13, 5
    )

    "be sliceable" in {
      evenPage.slice(n = 5).map(page =>
        page.text.flatten.toSet.size shouldBe 1
      )
    }

    "be spliceable" in {
      evenPage.slice(n = 5).reduce(_.splice(_)) shouldBe evenPage
    }
  }

}
