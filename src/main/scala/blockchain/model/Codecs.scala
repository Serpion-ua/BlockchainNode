package blockchain.model

import io.circe.Decoder
import io.circe.generic.semiauto._
import blockchain.model.Block
import io.circe.generic.auto._

object Codecs {
  implicit val blockDecoder: Decoder[Block] = deriveDecoder
}
