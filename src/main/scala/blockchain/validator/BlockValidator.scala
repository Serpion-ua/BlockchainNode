package blockchain.validator

import blockchain.model.Block

object BlockValidator {
  //TODO add block data verification. Right now we could skip it because only current node could mint blocks
  def verify(block: Block): Boolean = true
}
