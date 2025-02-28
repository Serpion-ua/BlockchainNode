package blockchain

import blockchain.ShowInstances._
import blockchain.model.Transaction.TransactionId
import blockchain.model.{BlockBody, BlockId, Transaction}
import blockchain.state.{BlockTreeState, BlocksTree}
import cats.effect.{Async, Ref, Sync}
import cats.implicits._
import org.typelevel.log4cats.Logger

//Storage for available transactions for minting. No verification is performed for added transaction(s)
//TODO add expiration for transactions
trait MemoryPool[F[_]] {
  def addTransaction(tx: Transaction): F[Unit]
  def getAllTransactionsAt(blockId: BlockId): F[Map[TransactionId, Transaction]]
}

object MemoryPool {
  def make[F[_]: Async: Logger](
      currentBlockId: BlockId,
      blocksTree: BlocksTree[F],
      blockBodiesStore: DataStore[F, BlockId, BlockBody], //must contain ALL blocks which are present in blocksTree
      initialState: Map[TransactionId, Transaction] = Map.empty
  ): F[MemoryPool[F]] = {
    for {
      state            <- Ref.of(initialState)
      memPoolTreeState <- MemoryPoolBlockTreeState.make(currentBlockId, state, blocksTree, blockBodiesStore)
      memPool = new MemoryPool[F] {
        override def addTransaction(tx: Transaction): F[Unit] =
          Logger[F].debug(s"Add transaction ${tx.id} to memory pool") >>
            state.update(txs => txs + (tx.id -> tx))

        override def getAllTransactionsAt(blockId: BlockId): F[Map[TransactionId, Transaction]] = {
          Logger[F].debug(s"Request to get all txs at $blockId") >>
            memPoolTreeState.useStateAt(blockId)(txs => txs.get)
        }
      }
    } yield memPool
  }
}

private object MemoryPoolBlockTreeState {
  private type MemPoolState[F[_]] = Ref[F, Map[TransactionId, Transaction]]

  def make[F[_]: Async: Logger](
      currentBlockId: BlockId,
      currentStateRef: MemPoolState[F],
      blocksTree: BlocksTree[F],
      blockDataStore: DataStore[F, BlockId, BlockBody]
  ): F[BlockTreeState[F, MemPoolState[F]]] =
    BlockTreeState.make[F, MemPoolState[F]](currentBlockId.pure[F], currentStateRef.pure[F], apply(blockDataStore), blocksTree)

  private def apply[F[_]: Sync: Logger](
      blockBodiesStore: DataStore[F, BlockId, BlockBody]
  )(blockId: BlockId, state: MemPoolState[F]): F[MemPoolState[F]] = {
    for {
      txIdsToRemove <- blockBodiesStore.get(blockId).map(_.get.txIds) //TODO Process impossible NONE
      _             <- Logger[F].trace(show"Apply block $blockId to Memory pool state by removing txs: $txIdsToRemove")
      _             <- state.update(txsMap => txsMap.removedAll(txIdsToRemove))
    } yield state
  }
}
