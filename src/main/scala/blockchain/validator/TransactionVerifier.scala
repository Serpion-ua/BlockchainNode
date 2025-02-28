package blockchain.validator

import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeyOps
import blockchain.ledger.{Ledger, LedgerData}
import blockchain.model.Transaction
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

object TransactionVerifier {
  def verifyTransaction(tx: Transaction, ledgerData: LedgerData, crypto: BlockchainKeyOps): Either[InvalidTransaction, Transaction] = {
    for {
      _ <- TransactionSyntaxValidation.verifyTransaction(tx, crypto)
      _ <- TransactionSemanticValidator.verifyTransaction(tx, ledgerData)
    } yield tx
  }
}
