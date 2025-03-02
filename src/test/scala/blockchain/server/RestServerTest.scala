package blockchain.server

import blockchain.Config.AppConfig
import blockchain.ShowInstances._
import blockchain.crypto.{BlockchainKeyOps, BlockchainKeyOpsSHA256WithECDSA}
import blockchain.model.Transaction.CoinAmount
import blockchain.model.{AccountData, AccountNonce, Block, Transaction}
import blockchain.validator.{IncorrectNonce, InvalidTransaction}
import blockchain.{ChainGenerator, ChainGeneratorState}
import cats.effect._
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.http4s.Status.Conflict
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.io._
import org.http4s.dsl.io.POST
import org.http4s.implicits._
import org.typelevel.ci._
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import pureconfig.ConfigSource
import pureconfig.generic.auto._

class RestServerTest extends CatsEffectSuite with ScalaCheckEffectSuite {
  implicit val loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]
  implicit val crypto: BlockchainKeyOps         = BlockchainKeyOpsSHA256WithECDSA
  val config: AppConfig                         = ConfigSource.default.loadOrThrow[AppConfig]
  private val defaultHeader                     = Header.Raw(ci"Content-Type", "application/json")

  test("POST /transaction should try to add transaction to memory pool with result success") {
    val restHandler = new ServiceLayer[IO] {
      override def addTransaction(tx: Transaction): IO[Either[InvalidTransaction, Transaction]] = IO(Right(tx))

      override def mintBlock(): IO[Block] = ???
    }

    val api    = new RestServer[IO](config.rest, restHandler)
    val client = Client.fromHttpApp(api.routes.orNotFound)

    val keyPair = crypto.generate
    val genesisAmount = CoinAmount(1000000)
    val keyAndAmount = Seq((keyPair, AccountData.default.copy(amount = genesisAmount)))
    val (_, chainState) = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, keyPair)
    val (blocks, _) = ChainGenerator.generateBlocks(chainState, 5, keyPair)
    val tx              = blocks.flatMap(_.body.txs).head
    val requestBody     = tx.asJson
    val path            = s"/${RestApiPaths.addTransaction}"
    val request         = POST(requestBody, Uri.unsafeFromString(path)).withHeaders(defaultHeader)

    for {
      response <- client.run(request).use(IO.pure)
      _ = assert(response.status.isSuccess)
    } yield ()
  }

  test("POST /transaction should try to add transaction to memory pool with result failed") {
    implicit val errorEncoder: EntityDecoder[IO, InvalidTransaction] = jsonOf[IO, InvalidTransaction]

    val keyPair = crypto.generate
    val genesisAmount = CoinAmount(1000000)
    val keyAndAmount = Seq((keyPair, AccountData.default.copy(amount = genesisAmount)))
    val (_, chainState) = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, keyPair)
    val (blocks, _) = ChainGenerator.generateBlocks(chainState, 5, keyPair)
    val tx              = blocks.flatMap(_.body.txs).head

    val txResponse = IncorrectNonce(tx.id, AccountNonce(0), AccountNonce(1))
    val restHandler = new ServiceLayer[IO] {
      override def addTransaction(tx: Transaction): IO[Either[InvalidTransaction, Transaction]] =
        IO(Left(txResponse))
      override def mintBlock(): IO[Block] = ???
    }

    val api    = new RestServer[IO](config.rest, restHandler)
    val client = Client.fromHttpApp(api.routes.orNotFound)

    val requestBody = tx.asJson
    val path        = s"/${RestApiPaths.addTransaction}"
    val request     = POST(requestBody, Uri.unsafeFromString(path)).withHeaders(defaultHeader)

    for {
      response   <- client.run(request).use(IO.pure)
      responseAs <- response.as[InvalidTransaction]
      _ = assert(response.status == Conflict)
      _ = assert(txResponse == responseAs)
    } yield ()
  }

  test("POST /block should mint block") {
    implicit val blockDecoder: EntityDecoder[IO, Block] = jsonOf[IO, Block]

    val keyPair = crypto.generate
    val genesisAmount = CoinAmount(1000000)
    val keyAndAmount = Seq((keyPair, AccountData.default.copy(amount = genesisAmount)))
    val (_, chainState) = ChainGeneratorState.fromKeysAndAmounts(keyAndAmount, keyPair)
    val (blocks, _) = ChainGenerator.generateBlocks(chainState, 2, keyPair)
    val blockToMint     = blocks.head

    val restHandler = new ServiceLayer[IO] {
      override def addTransaction(tx: Transaction): IO[Either[InvalidTransaction, Transaction]] = ???

      override def mintBlock(): IO[Block] = blockToMint.pure[IO]
    }

    val api    = new RestServer[IO](config.rest, restHandler)
    val client = Client.fromHttpApp(api.routes.orNotFound)

    val path    = s"/${RestApiPaths.mintBlock}"
    val request = POST(Uri.unsafeFromString(path)).withHeaders(defaultHeader)

    for {
      response   <- client.run(request).use(IO.pure)
      responseAs <- response.as[Block]
      _ = assert(response.status.isSuccess)
      _ = assert(blockToMint == responseAs)
    } yield ()
  }
}
