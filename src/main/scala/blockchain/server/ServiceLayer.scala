package blockchain.server

import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeyOps
import blockchain.forging.BlockProduction
import blockchain.ledger.Ledger
import blockchain.model.{Block, Transaction}
import blockchain.validator.{InvalidTransaction, TransactionVerifier}
import blockchain.{LocalChain, MemoryPool}
import cats.data.EitherT
import cats.effect._
import cats.implicits._
import org.typelevel.log4cats.Logger

trait ServiceLayer[F[_]] {
  def addTransaction(tx: Transaction): F[Either[InvalidTransaction, Transaction]]
  def mintBlock(): F[Block]
}

class ServiceLayerImpl[F[_]: Async: Logger](
    localChain: LocalChain[F],
    ledger: Ledger[F],
    memoryPool: MemoryPool[F],
    blockProduction: BlockProduction[F],
    crypto: BlockchainKeyOps
) extends ServiceLayer[F] {
  def addTransaction(tx: Transaction): F[Either[InvalidTransaction, Transaction]] = {
    checkTransactionOnLocalHead(tx)
      .flatTap(r => EitherT.liftF(memoryPool.addTransaction(r)))
      .value
  }

  //It is generally not to safe do such verification in MemoryPool because DDoS attack could be done on MemoryPool
  private def checkTransactionOnLocalHead(tx: Transaction): EitherT[F, InvalidTransaction, Transaction] = {
    for {
      head          <- EitherT.right(localChain.head)
      ledgerState   <- EitherT.right(ledger.stateAt(head))
      memoryPoolTxs <- EitherT.right(memoryPool.getAllTransactionsAt(head).map(_.values).map(_.toSeq))
      updatedState = memoryPoolTxs.foldLeft(ledgerState)((ledger, tx) => ledger.applyTransaction(tx))
      res <- EitherT.fromEither[F](TransactionVerifier.verifyTransaction(tx, updatedState, crypto))
    } yield res
  }

  override def mintBlock(): F[Block] = blockProduction.produceBlock
}
