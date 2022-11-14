package champash.thecutup

import scala.util.Random
import CoolOps._

object CuttingMachine {

  sealed trait MixMode

  case object StripedPage extends MixMode

  case object FreeLinear extends MixMode

  case object NonLinear extends MixMode

  sealed trait CompressionMode

  case object Uncompressed extends CompressionMode

  case object CompressVertically extends CompressionMode

  case object CompressHorizontally extends CompressionMode

  case object CompressAll extends CompressionMode

  def toPages(text: String, w: Int, h: Int, t: Int, g: Int, compressionMode: CompressionMode): List[Page] = {
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
          g: Int,
          n: Int,
          cutMode: MixMode,
          spacingMode: CompressionMode,
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
