package blockchain

import blockchain.Config.AppConfig
import blockchain.Genesis._
import blockchain.ShowInstances._
import blockchain.crypto.Signable.TransactionSigning
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsSHA256WithECDSA}
import blockchain.forging.BlockProduction
import blockchain.ledger.{Ledger, LedgerData}
import blockchain.model.Transaction.CoinAmount
import blockchain.model._
import blockchain.server.{RestServer, ServiceLayerImpl}
import blockchain.state.BlocksTree
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import fs2.Stream
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import org.typelevel.log4cats.{Logger, LoggerFactory}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import scala.concurrent.duration.DurationInt

object Main extends ResourceApp.Forever {
  type F[A] = IO[A]
  //TODO crypto shall be parameter, so we could easily change used keys types
  implicit val crypto: BlockchainKeyOps        = BlockchainKeyOpsSHA256WithECDSA
  implicit val loggerFactory: LoggerFactory[F] = Slf4jFactory.create[F]
  implicit val logger: Logger[F]               = Slf4jLogger.getLoggerFromName[F]("NodeMain")

  //TODO Add persistence, i.e. save/load blockchain state to/from disk
  override def run(args: List[String]): Resource[F, Unit] = {
    //TODO genesis keys, as well as signing key shall be put into config file instead of hardcoding them
    val config: AppConfig = ConfigSource.default.loadOrThrow[AppConfig]

    for {
      _ <- logger.info("Starting node").toResource
      headersStore = new InMemoryDataStore[F, BlockId, BlockHeader](Map(GenesisBlockId -> GenesisHeader))
      bodiesStore  = new InMemoryDataStore[F, BlockId, BlockBody](Map(GenesisBlockId -> GenesisBlockBody))

      blocksTree <- BlocksTree.make[F](IndexedSeq(GenesisParentId)).toResource
      _          <- blocksTree.parentOf(GenesisParentId, GenesisBlockId).toResource

      localChain <- LocalChain.make[F](GenesisParentId)
      adoptions  <- localChain.adoptions.toResource
      _          <- localChain.adopt(GenesisBlockId).toResource
      _          <- Logger[F].info(show"Start blockchain with genesis block ${GenesisBlock.id}").toResource

      memoryPool <- MemoryPool.make[F](GenesisParentId, blocksTree, bodiesStore).toResource
      ledger     <- Ledger.make[F](GenesisParentId, LedgerData.empty, blocksTree, bodiesStore).toResource

      blockSigningKey = GenesisKey1.privateKey
      blockProduction <- BlockProduction
        .make[F](localChain, blockSigningKey, headersStore, bodiesStore, blocksTree, memoryPool, ledger)

      restHandler = new ServiceLayerImpl[F](localChain, ledger, memoryPool, blockProduction, crypto)
      _ <- new RestServer(config.rest, restHandler).runServer
      _ <- logger.info("Started rest server").toResource
    } yield ExitCode.Success
  }
}
