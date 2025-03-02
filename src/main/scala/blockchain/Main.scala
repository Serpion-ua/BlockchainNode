package blockchain

import blockchain.Config.{AppConfig, GenesisConfig}
import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeys.BlockchainPrivateKey
import blockchain.crypto.Signable.TransactionSigning
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsSHA256WithECDSA}
import blockchain.forging.BlockProduction
import blockchain.ledger.{Ledger, LedgerData}
import blockchain.model._
import blockchain.server.{RestServer, ServiceLayerImpl}
import blockchain.state.BlocksTree
import cats.effect._
import cats.implicits._
import io.circe.Decoder
import io.circe.parser.decode
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import org.typelevel.log4cats.{Logger, LoggerFactory}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import scala.io.Source

object Main extends ResourceApp.Forever {
  type F[A] = IO[A]
  implicit val loggerFactory: LoggerFactory[F] = Slf4jFactory.create[F]
  implicit val logger: Logger[F]               = Slf4jLogger.getLoggerFromName[F]("NodeMain")

  override def run(args: List[String]): Resource[F, Unit] = {
    val config: AppConfig = ConfigSource.default.loadOrThrow[AppConfig]

    val runOrError: Either[Exception, Resource[F, Unit]] =
      for {
        crypto <- Either.right(BlockchainKeyOpsSHA256WithECDSA)
        genesisBlock <- loadGenesisBlock(config.genesis)
        nodeKey <- loadNodeKey(config.crypto, crypto)
      } yield runNode(config, genesisBlock, nodeKey, BlockchainKeyOpsSHA256WithECDSA)

    runOrError.fold(e => Logger[F].error(s"Failed to start node due $e").toResource, identity)
  }

  private def loadGenesisBlock(genesis: GenesisConfig): Either[Exception, Block] = {
    implicit val blockDecoder: Decoder[Block] = Codecs.blockDecoder

    val notFoundError = new IllegalArgumentException(s"Failed to find json file in path ${genesis.path}")

    Either
      .fromOption(Option(getClass.getResourceAsStream(genesis.path)), notFoundError)
      .map(Source.fromInputStream)
      .map(_.mkString)
      .flatMap(decode[Block])
  }

  private def loadNodeKey(config: Config.CryptoConfig, crypto: BlockchainKeyOps): Either[Exception, BlockchainPrivateKey] = {
    val maybeKey = BlockchainPrivateKey(config.nodeKey)
    Either.cond(crypto.isValidPrivateKey(maybeKey), maybeKey, new IllegalArgumentException(s"Failed to load valid private key"))
  }

  //TODO Add persistence, i.e. save/load blockchain state to/from disk
  def runNode(
               config: AppConfig,
               genesisBlock: Block,
               blockSigningKey: BlockchainPrivateKey,
               keyOps: BlockchainKeyOps
             ): Resource[F, Unit] = {
    implicit val crypto: BlockchainKeyOps = keyOps

    for {
      _ <- logger.info("Starting node").toResource

      genesisId = genesisBlock.id
      genesisParentId = genesisBlock.header.parentId
      headersStore = new InMemoryDataStore[F, BlockId, BlockHeader](Map(genesisId -> genesisBlock.header))
      bodiesStore = new InMemoryDataStore[F, BlockId, BlockBody](Map(genesisId -> genesisBlock.body))

      blocksTree <- BlocksTree.make[F](IndexedSeq(genesisParentId)).toResource
      _ <- blocksTree.parentOf(genesisParentId, genesisId).toResource

      localChain <- LocalChain.make[F](genesisParentId)
      adoptions  <- localChain.adoptions.toResource
      _ <- localChain.adopt(genesisId).toResource
      _ <- Logger[F].info(show"Start blockchain with genesis block $genesisId").toResource

      memoryPool <- MemoryPool.make[F](genesisParentId, blocksTree, bodiesStore).toResource
      ledger <- Ledger.make[F](genesisParentId, LedgerData.empty, blocksTree, bodiesStore).toResource

      blockProduction <- BlockProduction
        .make[F](localChain, blockSigningKey, headersStore, bodiesStore, blocksTree, memoryPool, ledger)

      restHandler = new ServiceLayerImpl[F](localChain, ledger, memoryPool, blockProduction, crypto)
      _ <- new RestServer(config.rest, restHandler).runServer
      _ <- logger.info("Started rest server").toResource
    } yield ExitCode.Success
  }
}
