package blockchain

import blockchain.Config.AppConfig
import blockchain.Genesis.{GenesisAmount, GenesisKey1}
import blockchain.ShowInstances._
import blockchain.crypto.Signable.TransactionSigning
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsSHA256WithECDSA}
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
  implicit val crypto: BlockchainKeyOps                                         = BlockchainKeyOpsSHA256WithECDSA
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

  //Signing is non-deterministic so we can't use equal
  private def equalNoSignatureCheck(left: Block, right: Block) = {
    val headersEquals = left.header.copy(signature = BlockSignature.empty) == right.header.copy(signature = BlockSignature.empty)
    val bodyEquals    = left.body == right.body
    headersEquals && bodyEquals
  }

  test("First block as expected") {
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val keyAndAmount    = Seq((Genesis.GenesisKey1, AccountData.default.copy(amount = Genesis.GenesisAmount)))
        val (_, chainState) = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, Genesis.GenesisKey1)
        val (blocks, _)     = ChainGenerator.generateBlocks(chainState, 2, Genesis.GenesisKey1)

        for {
          block0 <- mintBlock(client, blocks(0))
          _ = assert(equalNoSignatureCheck(block0, blocks(0)))
        } yield ()
      }
    }
  }

  test("Transaction after 20 blocks is accepted") {
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val keyAndAmount       = Seq((Genesis.GenesisKey1, AccountData.default.copy(amount = Genesis.GenesisAmount)))
        val (_, chainState)    = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, Genesis.GenesisKey1)
        val (blocks, endState) = ChainGenerator.generateBlocks(chainState, 20, Genesis.GenesisKey1)
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
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val keyAndAmount       = Seq((Genesis.GenesisKey1, AccountData.default.copy(amount = Genesis.GenesisAmount)))
        val (_, chainState)    = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, Genesis.GenesisKey1)
        val (blocks, endState) = ChainGenerator.generateBlocks(chainState, 5, Genesis.GenesisKey1)
        val tx                 = blocks.flatMap(_.body.txs).head
        val txKey              = endState.ledger.keys.filter(_.publicKey == tx.from).head
        for {
          tx1Res <- addTransactionRequest(client)(tx).flatMap(_.as[Transaction])
          _   = assert(tx1Res == tx)
          tx2 = tx1Res.copy(nonce = tx.nonce.nextNonce, amount = GenesisAmount).sign(txKey.privateKey)
          tx2Res <- addTransactionRequest(client)(tx2).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[NotEnoughFunds])
          _ = assert(tx2Res.required == GenesisAmount)
        } yield ()
      }
    }
  }

  test("Incorrect nonce") {
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(GenesisKey1.publicKey, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty)
          .sign(GenesisKey1.privateKey)

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
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(GenesisKey1.publicKey, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty)
          .sign(GenesisKey1.privateKey)
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
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(GenesisKey1.publicKey, to.publicKey, AccountNonce(1), CoinAmount(-1), TransactionSignature.empty)
          .sign(GenesisKey1.privateKey)

        for {
          txRes <- addTransactionRequest(client)(tx).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[NegativeCoinAmount])
          _ = assert(txRes.txId == tx.id)
        } yield ()
      }
    }
  }

  test("Incorrect from address") {
    Main.run(List.empty).use { _ =>
      EmberClientBuilder.default[F].build.use { client =>
        val to = crypto.generate
        val tx = Transaction(Account.dead.value, to.publicKey, AccountNonce(1), CoinAmount(1), TransactionSignature.empty)
          .sign(GenesisKey1.privateKey)

        for {
          txRes <- addTransactionRequest(client)(tx).flatMap(_.as[InvalidTransaction]).map(_.asInstanceOf[IncorrectFromAddress])
          _ = assert(txRes.txId == tx.id)
        } yield ()
      }
    }
  }

  test("Unknown from address") {
    Main.run(List.empty).use { _ =>
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
