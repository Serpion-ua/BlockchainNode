package blockchain.ledger

import blockchain.ShowInstances._
import blockchain.model._
import blockchain.state.{BlockTreeState, BlocksTree}
import blockchain.DataStore
import cats.effect.{Async, Ref, Sync}
import cats.implicits._
import org.typelevel.log4cats.Logger

//Allow to get Global ledger state at come point on the chain,
// i.e. it contains information about all accounts including balances and nonce
//No verification done here because we assume that all data on the chain is correct and verified before
trait Ledger[F[_]] {
  def stateAt(blockId: BlockId): F[LedgerData]
}

object Ledger {
  def make[F[_]: Async: Logger](
      currentBlockId: BlockId,
      initialState: LedgerData,
      blocksTree: BlocksTree[F],
      blockBodiesStore: DataStore[F, BlockId, BlockBody]
  ): F[Ledger[F]] = {
    for {
      state            <- Ref.of(initialState)
      memPoolTreeState <- LedgerBlocksTreeState.make(currentBlockId, state, blocksTree, blockBodiesStore)
    } yield new Ledger[F] {
      override def stateAt(blockId: BlockId): F[LedgerData] =
        memPoolTreeState.useStateAt(blockId)(_.get)
    }
  }
}

object LedgerBlocksTreeState {
  private type LedgerState[F[_]] = Ref[F, LedgerData]

  def make[F[_]: Async: Logger](
      currentBlockId: BlockId,
      currentStateRef: LedgerState[F],
      blocksTree: BlocksTree[F],
      blockDataStore: DataStore[F, BlockId, BlockBody]
  ): F[BlockTreeState[F, LedgerState[F]]] = {
    BlockTreeState.make[F, LedgerState[F]](currentBlockId.pure[F], currentStateRef.pure[F], apply(blockDataStore), blocksTree)
  }

  private def apply[F[_]: Sync: Logger](
      blockBodyDataStore: DataStore[F, BlockId, BlockBody]
  )(blockId: BlockId, state: LedgerState[F]): F[LedgerState[F]] = {
    for {
      body <- blockBodyDataStore.get(blockId).map(_.get)
      txs  <- body.txs.pure[F]
      _    <- state.update(txs.foldLeft(_)(applyTransaction))
    } yield state
  }

  private def applyTransaction(ledgerData: LedgerData, tx: Transaction): LedgerData = ledgerData.applyTransaction(tx)
}
