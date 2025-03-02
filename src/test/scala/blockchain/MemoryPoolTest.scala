package blockchain

import blockchain.Generators._
import blockchain.crypto.BlockchainKeys.BlockchainKeyPair
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsSHA256WithECDSA}
import blockchain.model.{AccountData, BlockBody, BlockId}
import blockchain.state.BlocksTree
import cats.effect.IO
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class MemoryPoolTest extends CatsEffectSuite with ScalaCheckEffectSuite {
  type F[A] = IO[A]
  implicit val logger: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("MemoryPoolTest")
  implicit val crypto: BlockchainKeyOps = BlockchainKeyOpsSHA256WithECDSA

  test("Memory pool shall return correct transactions set") {
    PropF.forAllF { params: (BlockId, BlockchainKeyPair, AccountData) =>
      val (genesisId: BlockId, genesisKeys: BlockchainKeyPair, genesisAccountData: AccountData) = params

      val keyAndAmount = Seq((genesisKeys, genesisAccountData))
      val (genesisBlock, chainState) = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, genesisKeys)
      val (blocks, _) = ChainGenerator.generateBlocks(chainState, 5, genesisKeys)
      val genesisParentId = genesisBlock.header.parentId
      val idToBlockBody = blocks.map(block => block.id -> block.body).toMap
      val blockIds = blocks.map(_.id).toIndexedSeq
      val dataStore = new InMemoryDataStore[F, BlockId, BlockBody](idToBlockBody)
      val allTxs = blocks.flatMap(_.body.txs)
      for {
        blocksTree <- BlocksTree.make[F](IndexedSeq(genesisParentId))
        _ <- blocksTree.parentOf(genesisParentId, genesisId)
        _ <- blocksTree.parentOf(genesisId, blocks.head.id)
        _ <- blockIds.sliding(2).toSeq.traverse(parentChild => blocksTree.parentOf(parentChild.head, parentChild.last))
        _ <- dataStore.put(genesisId, genesisBlock.body)
        memoryPool <- MemoryPool.make(genesisId, blocksTree, dataStore)
        _ <- allTxs.traverse(memoryPool.addTransaction)
        txInMemoryPoolGenesis <- memoryPool.getAllTransactionsAt(genesisId).map(_.values)
        _ <- assert(txInMemoryPoolGenesis.toSet == allTxs.toSet).pure[F]
        lastBlockTxIds = blocks.last.allTxsIds.toSet
        preLastBlockMemoryPoolTxs <- memoryPool.getAllTransactionsAt(blocks.init.last.id).map(_.keySet)
        _ <- assert(lastBlockTxIds == preLastBlockMemoryPoolTxs).pure[F]
        _ <- assertIOBoolean(memoryPool.getAllTransactionsAt(blocks.last.id).map(_.isEmpty))
      } yield ()
    }
  }
}
