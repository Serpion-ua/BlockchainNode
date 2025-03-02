package blockchain.crypto

import blockchain.crypto.BlockchainKeys.{BlockchainKeyPair, BlockchainPrivateKey, BlockchainPublicKey}

object BlockchainKeys {
  case class BlockchainPrivateKey(value: String) extends AnyVal
  case class BlockchainPublicKey(value: String)  extends AnyVal
  case class BlockchainKeyPair(privateKey: BlockchainPrivateKey, publicKey: BlockchainPublicKey)
}

trait BlockchainKeyOps {
  def sign(secret: BlockchainPrivateKey, messageBytes: Array[Byte]): String
  def verify(public: BlockchainPublicKey, messageBytes: Array[Byte], signedMessageBytes: String): Boolean
  def generate: BlockchainKeyPair
  def isValidPublicKey(public: BlockchainPublicKey): Boolean

  def isValidPrivateKey(public: BlockchainPrivateKey): Boolean

}
