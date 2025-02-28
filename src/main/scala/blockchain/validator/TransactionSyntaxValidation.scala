package blockchain.validator
import blockchain.crypto.BlockchainKeyOps
import blockchain.crypto.Signable._
import blockchain.model.Transaction

object TransactionSyntaxValidation {
  def verifyTransaction(tx: Transaction, crypto: BlockchainKeyOps): Either[InvalidTransaction, Transaction] = {
    for {
      _ <- Either.cond(tx.amount.isValid, (), NegativeCoinAmount(tx.id, tx.amount))
      _ <- Either.cond(crypto.isValidPublicKey(tx.from), (), IncorrectFromAddress(tx.id, tx.from))
      _ <- Either.cond(crypto.verify(tx.from, tx.toSign, tx.signature.value), (), IncorrectSignature(tx.id))
    } yield tx
  }
}
