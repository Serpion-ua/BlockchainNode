package blockchain.model

import blockchain.crypto.BlockchainKeys.BlockchainPublicKey
import blockchain.model.Transaction.{CoinAmount, TxAddress}


case class Account(value: TxAddress) {
  def isNotDead: Boolean = this != Account.dead
}
object Account {
  //account used for as source of coins in genesis block, burning coins, and possible forging reward
  val dead: Account = Account(BlockchainPublicKey("DEAD_END"))
}

case class AccountNonce(value: Long) extends AnyVal {
  def nextNonce: AccountNonce = AccountNonce(value + 1)
  def prevNonce: AccountNonce = AccountNonce(value - 1)
  def >(other: AccountNonce): Boolean   = value > other.value
}

object AccountNonce {
  val initialNonce: AccountNonce = AccountNonce(0)
}


case class AccountData(amount: CoinAmount, lastUsedNonce: AccountNonce)

object AccountData {
  val default: AccountData = AccountData(CoinAmount(0), AccountNonce(0))
}