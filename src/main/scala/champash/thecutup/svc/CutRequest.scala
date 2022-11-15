package champash.thecutup.svc

case class CutRequest(texts: List[String],
                      width: Int,
                      height: Int,
                      tabSize: Int,
                      maybeGive: Option[Int],
                      numSlices: Int,
                      cutMode: String,
                      spacingMode: String,
                      compressionMode: String,
                      maybeRandomSeed: Option[Int])