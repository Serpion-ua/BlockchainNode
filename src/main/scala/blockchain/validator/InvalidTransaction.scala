package blockchain.validator

import blockchain.model.{Account, AccountNonce}
import blockchain.model.Transaction.{CoinAmount, TransactionId, TxAddress}

sealed trait InvalidTransaction extends Throwable
case class UnknownAccount(txId: TransactionId, fromAccount: Account) extends InvalidTransaction
case class NotEnoughFunds(txId: TransactionId,actual: CoinAmount, required: CoinAmount) extends InvalidTransaction
case class IncorrectNonce(txId: TransactionId,actual: AccountNonce, required: AccountNonce) extends InvalidTransaction
case class NegativeCoinAmount(txId: TransactionId,amount: CoinAmount) extends InvalidTransaction
case class IncorrectSignature(txId: TransactionId) extends InvalidTransaction
case class IncorrectFromAddress(txId: TransactionId, from: TxAddress) extends InvalidTransaction