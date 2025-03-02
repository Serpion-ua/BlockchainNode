package blockchain.ledger

import blockchain.Generators._
import blockchain.crypto.BlockchainKeys.BlockchainKeyPair
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsSHA256WithECDSA}
import blockchain.model._
import blockchain.state.BlocksTree
import blockchain.{ChainGenerator, ChainGeneratorState, InMemoryDataStore}
import cats.effect.IO
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class LedgerTest extends CatsEffectSuite with ScalaCheckEffectSuite {
  type F[A] = IO[A]
  implicit val logger: Logger[IO]       = Slf4jLogger.getLoggerFromName[IO]("LedgerTest")
  implicit val crypto: BlockchainKeyOps = BlockchainKeyOpsSHA256WithECDSA

  test("All data shall be applied to the ledger") {
    PropF.forAllF { params: (BlockId, BlockchainKeyPair, AccountData) =>
      val (genesisId: BlockId, genesisKeys: BlockchainKeyPair, genesisAccountData: AccountData) = params

      val keyAndAmount               = Seq((genesisKeys, genesisAccountData))
      val (genesisBlock, chainState) = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, genesisKeys)
      val geneBlockParentId = genesisBlock.header.parentId
      val (blocks, _)                = ChainGenerator.generateBlocks(chainState, 5, genesisKeys)
      val idToBlockBody              = blocks.map(block => block.id -> block.body).toMap
      val blockIds                   = blocks.map(_.id).toIndexedSeq
      val dataStore                  = new InMemoryDataStore[F, BlockId, BlockBody](idToBlockBody)
      val allTxs                     = blocks.flatMap(_.body.txs)
      val accountToNonce =
        allTxs.groupMapReduce(_.from)(_ => 1)(_ + _).map { case (k, v) => (Account(k), AccountNonce(v)) }.filter(_._1.isNotDead)
      for {
        blocksTree <- BlocksTree.make[F](IndexedSeq(geneBlockParentId))
        _ <- blocksTree.parentOf(geneBlockParentId, genesisId)
        _          <- blocksTree.parentOf(genesisId, blocks.head.id)
        _          <- blockIds.sliding(2).toSeq.traverse(parentChild => blocksTree.parentOf(parentChild.head, parentChild.last))
        _          <- dataStore.put(genesisId, genesisBlock.body)
        ledger <- Ledger.make(geneBlockParentId, LedgerData.empty, blocksTree, dataStore)
        ledgerData <- ledger.stateAt(blockIds.last).map(_.accountToData)
        lastBlockchainAmount = ledgerData.map(_._2.amount.value).sum
        genesisValue         = genesisAccountData.amount.value
        _               <- assert(lastBlockchainAmount == genesisValue).pure[F]
        ledgerDataNonce <- ledgerData.view.mapValues(_.lastUsedNonce).filter(_._2.value > 0).toMap.pure[F]
        _ = assert(accountToNonce == ledgerDataNonce)
      } yield ()
    }
  }
}
