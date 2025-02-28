package blockchain.crypto

import blockchain.crypto.BlockchainKeys.BlockchainPrivateKey
import blockchain.model.Transaction.{CoinAmount, TxAddress}
import blockchain.model._

import java.nio.charset.StandardCharsets

//Describe which data shall be signed, as well as how we sign Block and Transaction
trait Signable[A] {
  def toSign(a: A): Array[Byte]
}

object Signable {
  implicit val addressSignable: Signable[TxAddress] = a => a.value.getBytes(StandardCharsets.UTF_8.name())

  implicit val longSignable: Signable[Long] = l => BigInt(l).toByteArray

  implicit val nonceSignable: Signable[AccountNonce] = n => n.value.toSign

  implicit val amountSignable: Signable[CoinAmount] = a => a.value.toSign

  implicit val txSignable: Signable[Transaction] = tx => tx.from.toSign ++ tx.to.toSign ++ tx.nonce.toSign ++ tx.amount.toSign

  implicit val blockHeight: Signable[BlockHeight] = h => h.value.toSign

  implicit val blockIdSignable: Signable[BlockId] = id => id.value.toSign

  implicit val unsignedBlockHeaderSignable: Signable[UnsignedBlockHeader] = h =>
    h.height.toSign ++ h.txsHash.toSign ++ h.parentId.toSign

  implicit val blockHeaderSignable: Signable[BlockHeader] = h => h.height.toSign ++ h.txsHash.toSign ++ h.parentId.toSign

  implicit def seqSignable[A: Signable]: Signable[Seq[A]] = seq => seq.map(_.toSign).fold(Array.empty[Byte])(_ ++ _)

  implicit class SignableSyntax[A: Signable](a: A) {
    def toSign: Array[Byte] = {
      val signable: Signable[A] = implicitly[Signable[A]]
      signable.toSign(a)
    }
  }

  implicit class TransactionSigning(tx: Transaction) {
    def sign(secret: BlockchainPrivateKey)(implicit crypto: BlockchainKeyOps): Transaction = {
      val signature = TransactionSignature(crypto.sign(secret, tx.toSign))
      tx.copy(signature = signature)
    }
  }

  implicit class UnsignedBlockSigning(block: UnsignedBlockHeader) {
    def sign(secret: BlockchainPrivateKey)(implicit crypto: BlockchainKeyOps): BlockHeader = {
      val signature = BlockSignature(crypto.sign(secret, block.toSign))
      BlockHeader(block.height, block.parentId, block.txsHash, signature)
    }
  }

}
