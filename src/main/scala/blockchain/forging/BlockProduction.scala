package blockchain.forging

import blockchain.ShowInstances._
import blockchain._
import blockchain.crypto.BlockchainKeyOps
import blockchain.crypto.BlockchainKeys.BlockchainPrivateKey
import blockchain.ledger.Ledger
import blockchain.model.{Block, BlockBody, BlockHeader, BlockId}
import blockchain.state.BlocksTree
import cats.effect.implicits._
import cats.effect.{Async, Resource}
import cats.implicits._
import org.typelevel.log4cats.Logger

trait BlockProduction[F[_]] {
  def produceBlock: F[Block]
}

//All data stores must contain data for ALL blocks which are present in blocksTree
object BlockProduction {
  def make[F[_]: Async: Logger](
      localChain: LocalChain[F],
      signingKey: BlockchainPrivateKey,
      headersStore: DataStore[F, BlockId, BlockHeader],
      blockBodiesStore: DataStore[F, BlockId, BlockBody],
      blocksTree: BlocksTree[F],
      memoryPool: MemoryPool[F],
      ledger: Ledger[F]
  )(implicit crypto: BlockchainKeyOps): Resource[F, BlockProduction[F]] = {
    def adoptForgedBlock(block: Block): F[Unit] = {
      for {
        _ <- Logger[F].info(show"Start to adopt locally forged block ${block.id}")
        _ <- headersStore.put(block.id, block.header)
        _ <- blockBodiesStore.put(block.id, block.body)
        _ <- blocksTree.parentOf(block.header.parentId, block.id)
        _ <- localChain.adopt(block.id)
      } yield ()
    }

    for {
      blockPacker  <- BlockPacker.make(memoryPool, ledger).toResource
      forger       <- Forger.make(signingKey)
      forgedBlocks <- forger.forgedBlocksStream.toResource
      _            <- forgedBlocks.evalTap(block => adoptForgedBlock(block)).compile.drain.background
    } yield new BlockProduction[F] {
      override def produceBlock: F[Block] = {
        for {
          _             <- Logger[F].info("Get request to produce block")
          currentHead   <- localChain.head
          currentHeader <- headersStore.get(currentHead).map(_.get)
          unsignedBlock <- blockPacker.buildBlockAt(currentHeader)
          signedBlock   <- forger.forgeBlock(unsignedBlock)
        } yield signedBlock
      }
    }
  }

}
