package blockchain.validator

import blockchain.ledger.LedgerData
import blockchain.model.{Account, Transaction}
import cats.implicits._

object TransactionSemanticValidator {
  def verifyTransaction(tx: Transaction, ledger: LedgerData): Either[InvalidTransaction, Transaction] = {
    for {
      fromAccount <- Either.right(Account(tx.from))
      fromData    <- Either.fromOption(ledger.accountToData.get(fromAccount), UnknownAccount(tx.id, fromAccount))
      _           <- Either.cond(fromData.amount >= tx.amount, (), NotEnoughFunds(tx.id, fromData.amount, tx.amount))
      _ <- Either.cond(
        fromData.lastUsedNonce.nextNonce == tx.nonce,
        (),
        IncorrectNonce(tx.id, tx.nonce, fromData.lastUsedNonce.nextNonce)
      )
    } yield tx
  }
}
