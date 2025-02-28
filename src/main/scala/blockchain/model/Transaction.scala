package blockchain.model

import blockchain.crypto.BlockchainKeys.BlockchainPublicKey
import blockchain.model.Transaction.{CoinAmount, TransactionId, TxAddress}

object Transaction {
  case class TransactionId(value: Long) extends AnyVal

  case class CoinAmount(value: Long) extends AnyVal {
    def isValid: Boolean = value >= 0

    def +(other: CoinAmount): CoinAmount = CoinAmount(value + other.value)
    def -(other: CoinAmount): CoinAmount = CoinAmount(value - other.value)
    def %(other: CoinAmount): CoinAmount = CoinAmount(value % other.value)
    def ==(other: CoinAmount): Boolean   = value == other.value
    def >=(other: CoinAmount): Boolean   = value >= other.value
    def <=(other: CoinAmount): Boolean   = value <= other.value
  }

  type TxAddress = BlockchainPublicKey
}

case class TransactionSignature(value: String) extends AnyVal

object TransactionSignature {
  val empty: TransactionSignature = TransactionSignature("")
}

case class Transaction(from: TxAddress, to: TxAddress, nonce: AccountNonce, amount: CoinAmount, signature: TransactionSignature) {
  //TODO use proper way to calculate transaction id, like using SHA-256
  lazy val id: TransactionId = TransactionId((from, to, amount).hashCode())
}
