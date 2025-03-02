package blockchain

import blockchain.ChainGeneratorState.genesisParentId
import blockchain.Config.AppConfig
import blockchain.Genesis.GenesisHeight
import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeys.BlockchainKeyPair
import blockchain.crypto.Signable.{TransactionSigning, UnsignedBlockSigning}
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsRSA}
import blockchain.model.Transaction.CoinAmount
import blockchain.model._
import blockchain.server.RestApiPaths
import blockchain.validator._
import cats.effect._
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import munit.CatsEffectSuite
import org.http4s.Method.POST
import org.http4s._
import org.http4s.circe._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.ci._
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import org.typelevel.log4cats.{Logger, LoggerFactory}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

//TODO do not use hardcoded SHA256WithECDSA keys for blockchain. It will allow to check signatures as well
class IntegrationTest extends CatsEffectSuite {
  type F[A] = IO[A]
  implicit val loggerFactory: LoggerFactory[F]                                  = Slf4jFactory.create[F]
  implicit val logger: Logger[F]                                                = Slf4jLogger.getLoggerFromName[F]("NodeMain")
  implicit val blockDecoder: EntityDecoder[IO, Block]                           = jsonOf[IO, Block]
  implicit val txDecoder: EntityDecoder[IO, Transaction]                        = jsonOf[IO, Transaction]
  implicit val InvalidTransactionDecoder: EntityDecoder[IO, InvalidTransaction] = jsonOf[IO, InvalidTransaction]
  implicit val notEnoughFundsDecoder: EntityDecoder[IO, NotEnoughFunds]         = jsonOf[IO, NotEnoughFunds]

  private val defaultHeader = Header.Raw(ci"Content-Type", "application/json")
  val config: AppConfig     = ConfigSource.default.loadOrThrow[AppConfig]
  private def buildUri(endPoint: String) = {
    val path = s"http://${config.rest.host}:${config.rest.port}/$endPoint"
    Uri.unsafeFromString(path)
  }

  private def addTransactionRequest(client: Client[F])(tx: Transaction): F[Response[F]] = {
    val requestBody = tx.asJson
    val uri         = buildUri(RestApiPaths.addTransaction)
    val request     = POST(requestBody, uri).withHeaders(defaultHeader)
    Logger[F].info(show"Try to add tx ${tx.id}") >>
      client.run(request).use(r => r.pure[F])
  }

  private def mintBlockRequest(client: Client[F]): F[Block] = {
    val uri     = buildUri(RestApiPaths.mintBlock)
    val request = POST(uri).withHeaders(defaultHeader)
    client.run(request).use(r => r.as[Block])
  }

  private def mintBlock(client: Client[F], block: Block): F[Block] = {
    for {
      _     <- block.body.txs.traverse(addTransactionRequest(client))
      block <- mintBlockRequest(client)
    } yield block
  }

  implicit val crypto: BlockchainKeyOps = BlockchainKeyOpsRSA
  private val genesisAmount = CoinAmount(1000000)
  private val genesisKey = crypto.generate
  private val createGenesis = {
    val genesisTx =
      Transaction(Account.dead.value, genesisKey.publicKey, AccountNonce.initialNonce, genesisAmount, TransactionSignature.empty)
    val genesisBlockBody: BlockBody = BlockBody(Seq(genesisTx))
    val genesisUnsignedHeader = UnsignedBlockHeader(GenesisHeight, genesisParentId, genesisBlockBody.txsHash)
    val genesisHeader = genesisUnsignedHeader.sign(genesisKey.privateKey)
    Block(genesisHeader, genesisBlockBody)
  }

  private val genesisBlock = createGenesis
  private val genesisTxPrivateKey = genesisKey.privateKey
  private val chainState = ChainGeneratorState.fromGenesisBlock(genesisBlock, Seq(genesisTxPrivateKey))

  private val genesisKeyPair = BlockchainKeyPair(genesisTxPrivateKey, genesisBlock.body.txs.head.to)

  test("First block as expected") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val (blocks, _) = ChainGenerator.generateBlocks(chainState, 2, genesisKeyPair)(crypto)

        for {
          block0 <- mintBlock(client, blocks.head)
          _ = println(show"${blocks.head}")
          _ = println(show"$block0")
          _ = assert(block0 == blocks.head)
        } yield ()
      }
    }
  }

  test("Transaction after 20 blocks is accepted") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val (blocks, endState) = ChainGenerator.generateBlocks(chainState, 20, genesisKeyPair)(crypto)
        val (tx, _)            = ChainGenerator.generateTransaction(endState)

        for {
          _     <- blocks.traverse(mintBlock(client, _))
          txRes <- addTransactionRequest(client)(tx).flatMap(_.as[Transaction])
          _ = assert(tx == txRes)
        } yield ()
      }
    }
  }

  test("Try double spend") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val (blocks, endState) = ChainGenerator.generateBlocks(chainState, 2, genesisKeyPair)(crypto)
        val tx                 = blocks.flatMap(_.body.txs).head
        val txKey              = endState.ledger.keys.filter(_.publicKey == tx.from).head
        for {
          tx1Res <- addTransactionRequest(client)(tx).flatMap(_.as[Transaction])
          _   = assert(tx1Res == tx)
          tx2 = tx1Res.copy(nonce = tx.nonce.nextNonce, amount = genesisAmount).sign(txKey.privateKey)
          tx2Res <- addTransactionRequest(client)(tx2).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[NotEnoughFunds])
          _ = assert(tx2Res.required == genesisAmount)
        } yield ()
      }
    }
  }

  test("Incorrect nonce") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(genesisKeyPair.publicKey, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty)
          .sign(genesisKeyPair.privateKey)

        for {
          tx1Res <- addTransactionRequest(client)(tx).flatMap(_.as[Transaction])
          _ = assert(tx1Res == tx)
          tx2Res <- addTransactionRequest(client)(tx).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[IncorrectNonce])
          _ = assert(tx2Res.required == tx.nonce.nextNonce)
          _ = assert(tx2Res.actual == tx.nonce)
        } yield ()
      }
    }
  }

  test("Incorrect signature") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(genesisKeyPair.publicKey, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty)
          .sign(genesisKeyPair.privateKey)
        val modifiedTx = tx.copy(signature = tx.signature.copy(value = tx.signature.value + "!"))

        for {
          tx1Res <- addTransactionRequest(client)(modifiedTx)
            .flatMap(_.as[InvalidTransaction])
            .map(_.asInstanceOf[IncorrectSignature])
          _ = assert(tx1Res.txId == modifiedTx.id)
          tx2Res <- addTransactionRequest(client)(tx).flatMap(_.as[Transaction])
          _ = assert(tx2Res == tx)
        } yield ()
      }
    }
  }

  test("Negative coin amount") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(genesisKeyPair.publicKey, to.publicKey, AccountNonce(1), CoinAmount(-1), TransactionSignature.empty)
          .sign(genesisKeyPair.privateKey)

        for {
          txRes <- addTransactionRequest(client)(tx).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[NegativeCoinAmount])
          _ = assert(txRes.txId == tx.id)
        } yield ()
      }
    }
  }

  test("Incorrect from address") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(Account.dead.value, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty)
          .sign(genesisKeyPair.privateKey)

        for {
          txRes <- addTransactionRequest(client)(tx).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[IncorrectFromAddress])
          _ = assert(txRes.txId == tx.id)
        } yield ()
      }
    }
  }

  test("Unknown from address") {
    Main.runNode(config, genesisBlock, genesisTxPrivateKey, crypto).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx =
          Transaction(to.publicKey, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty).sign(to.privateKey)

        for {
          txRes <- addTransactionRequest(client)(tx).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[UnknownAccount])
          _ = assert(txRes.txId == tx.id)
        } yield ()
      }
    }
  }

}
