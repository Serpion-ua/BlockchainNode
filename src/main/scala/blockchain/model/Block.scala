package blockchain.model

import blockchain.Genesis.GenesisHeight
import blockchain.model.Transaction.TransactionId

case class BlockId(value: Long) extends AnyVal

case class BlockHeight(value: Long) extends AnyVal {
  def childHeight: BlockHeight  = BlockHeight(value + 1)
  def parentHeight: BlockHeight = BlockHeight(value - 1) //What about parent of Genesis block?
  def isGenesis: Boolean        = value == GenesisHeight.value
}

case class Block(header: BlockHeader, body: BlockBody) {
  lazy val id: BlockId                   = header.id
  lazy val allTxsIds: Seq[TransactionId] = body.txs.map(_.id)
}

case class BlockBody(txs: Seq[Transaction]) {
  lazy val txIds: Seq[TransactionId] = txs.map(_.id)
  lazy val txsHash: Long             = txIds.hashCode() //TODO use proper hash function here
}

case class BlockSignature(value: String) extends AnyVal
object BlockSignature {
  val empty: BlockSignature = BlockSignature("")
}

//TODO Merkle tree shall be used instead of just txHash
//TODO we shall provide somehow public key to verify block signature in case of non PoW
case class BlockHeader(height: BlockHeight, parentId: BlockId, txsHash: Long, signature: BlockSignature) {
  //TODO use proper way to calculate transaction id, like SHA-256
  lazy val id: BlockId = BlockId((height, parentId, txsHash).hashCode())
}

case class UnsignedBlock(unsignedHeader: UnsignedBlockHeader, body: BlockBody)
case class UnsignedBlockHeader(height: BlockHeight, parentId: BlockId, txsHash: Long)
