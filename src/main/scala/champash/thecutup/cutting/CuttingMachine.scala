package champash.thecutup.cutting

import champash.thecutup.cutting.CoolOps._

import scala.util.Random

object CuttingMachine {

  sealed trait MixMode

  object MixMode {
    val Map: Map[String, MixMode] = Seq(StripedPage, FreeLinear, NonLinear).map(mm => mm.toString -> mm).toMap

    def apply(string: String): MixMode = {
      require(Map.contains(string), s"No MixMode with name $string")
      Map(string)
    }
  }

  // Take a slice from each page until you have N slices, then shuffle the result.
  case object StripedPage extends MixMode

  // Take a fully sliced page from each text and shuffle the result.
  // This will be broken into multiple pages of output.
  case object FreeLinear extends MixMode

  // Take all slices from all pages and shuffle the result.
  // This will be broken down into multiple pages of output.
  case object NonLinear extends MixMode

  sealed trait CompressionMode

  object CompressionMode {
    val Map: Map[String, CompressionMode] = Seq(Uncompressed, CompressVertically, CompressHorizontally, CompressAll).map(cm => cm.toString -> cm).toMap

    def apply(string: String): CompressionMode = {
      require(Map.contains(string), s"No CompressionMode with name $string")
      Map(string)
    }
  }

  // Allow whitespace on sides and bottom.
  case object Uncompressed extends CompressionMode

  // Remove empty lines.
  case object CompressVertically extends CompressionMode

  // Remove trailing spaces.
  case object CompressHorizontally extends CompressionMode

  // Remove empty lines and trailing spaces.
  case object CompressAll extends CompressionMode

  def toPages(text: String, w: Int, h: Int, t: Int, g: Option[Int], compressionMode: CompressionMode): List[Page] = {
    text.detab(t)
      .breakIntoLines(w, g)
      .flatMap(_.grouped(w))
      .map(_.maybePad(w, compressionMode))
      .grouped(h)
      .map(_.maybePadTo(h, " " * w, compressionMode))
      .toList
      .map(Page(_, w, h))
  }

  def cut(texts: List[String],
          w: Int,
          h: Int,
          t: Int,
          g: Option[Int],
          n: Int,
          cutMode: MixMode,
          compressionMode: CompressionMode,
          maybeRandom: Option[Random]): List[Page] = {
    val pagedTexts = texts.map(toPages(_, w, h, t, g, compressionMode))
    val slicedTexts = pagedTexts.map(_.slice(n))
    val interleaved = slicedTexts.interleave
    val groupSize = cutMode match {
      case StripedPage => n
      case FreeLinear => n * texts.length
      case NonLinear => interleaved.length
    }
    val grouped = interleaved.grouped(groupSize).toList
    val random = maybeRandom.getOrElse(Random)
    val shuffled = grouped.flatMap(random.shuffle(_))
    shuffled.splice(n)
  }

  // TODO: implement noNeighbors

}
