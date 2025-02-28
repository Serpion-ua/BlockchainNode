package blockchain.forging

import blockchain.MemoryPool
import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeyOps
import blockchain.ledger.{Ledger, TransactionsPruner}
import blockchain.model._
import cats.effect.Async
import cats.implicits._
import org.typelevel.log4cats.Logger

//Packing compatible transactions from memory pool to the block.
//All available transactions are packed to the block.
//Could be improved to pack only limited amount of transaction as well as make smart transactions choice for packing.
//If smart packing will be implemented then such packing shall be implemented in separate fiber in background
//because it is heavy computational
trait BlockPacker[F[_]] {
  def buildBlockAt(parent: BlockHeader): F[UnsignedBlock]
}

object BlockPacker {
  def make[F[_]: Async: Logger](memoryPool: MemoryPool[F], ledger: Ledger[F])(implicit
      crypto: BlockchainKeyOps
  ): F[BlockPacker[F]] = {
    val packer: BlockPacker[F] = new BlockPackerImpl[F](memoryPool, ledger)
    packer.pure[F]
  }
}

class BlockPackerImpl[F[_]: Async: Logger](memoryPool: MemoryPool[F], ledger: Ledger[F])(implicit crypto: BlockchainKeyOps)
    extends BlockPacker[F] {
  override def buildBlockAt(parentHeader: BlockHeader): F[UnsignedBlock] = {
    for {
      parentId       <- parentHeader.id.pure[F]
      _              <- Logger[F].debug(show"Start packing transactions to block with parent $parentId")
      txs            <- memoryPool.getAllTransactionsAt(parentId)
      _              <- Logger[F].debug(show"Got transaction from memory pool ${txs.values.toSeq} to block with parent $parentId")
      ledgerState    <- ledger.stateAt(parentId)
      //ALL transactions in memory pool in consistent state, i.e. we could skip that check.
      //However, verification in memory pool could be discarded, so better have it here as well
      filteredTxs    <- TransactionsPruner.pruneInvalidTxs(txs.values.toSeq, ledgerState, crypto).pure[F]
      unsignedBody   <- BlockBody(filteredTxs).pure[F]
      unsignedHeader <- UnsignedBlockHeader(parentHeader.height.childHeight, parentHeader.id, unsignedBody.txsHash).pure[F]
    } yield UnsignedBlock(unsignedHeader, unsignedBody)
  }

}
