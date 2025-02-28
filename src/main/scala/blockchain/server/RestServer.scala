package blockchain.server

import blockchain.Config.RestServerConfig
import blockchain.ShowInstances._
import blockchain.model.{Block, Transaction}
import cats.effect._
import cats.implicits._
import com.comcast.ip4s.{Host, Port}
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Server
import org.typelevel.log4cats.slf4j.{Slf4jFactory, Slf4jLogger}
import org.typelevel.log4cats.{Logger, LoggerFactory}

object RestApiPaths {
  val addTransaction = "transaction"
  val mintBlock      = "mint"
}

//Rest requests handler
class RestServer[F[_]: Async](config: RestServerConfig, service: ServiceLayer[F]) {
  implicit val loggerFactory: LoggerFactory[F] = Slf4jFactory.create[F]
  implicit val logger: Logger[F]               = Slf4jLogger.getLoggerFromName[F]("RestServer")

  private val dsl = Http4sDsl[F]
  import dsl._

  implicit val transactionDecoder: EntityDecoder[F, Transaction] = jsonOf[F, Transaction]
  implicit val blockDecoder: EntityEncoder[F, Block]             = jsonEncoderOf[Block]

  val routes: HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req @ POST -> Root / RestApiPaths.addTransaction => addTransaction(req)
      case req @ POST -> Root / RestApiPaths.mintBlock      => mintBlock(req)
    }
  }

  private def addTransaction(req: Request[F]): F[Response[F]] = {
    for {
      transaction <- req.as[Transaction]
      _ <- Logger[F].info(show"Requesting to add transaction ${transaction.id}")
      res         <- service.addTransaction(transaction)
      response <- res match {
        case Left(value) => Conflict(value.asJson)
        case Right(tx)   => Logger[F].info(show"Added transaction ${tx.id}") >> Ok(tx.asJson)
      }
    } yield response
  }

  private def mintBlock(req: Request[F]): F[Response[F]] = {
    for {
      mintedBlock <- service.mintBlock()
      response    <- Ok(mintedBlock.asJson)
    } yield response
  }

  //TODO handle incorrect config parameters, like negative port
  def runServer: Resource[F, Server] =
    EmberServerBuilder
      .default[F]
      .withPort(Port.fromInt(config.port).get)
      .withHost(Host.fromString(config.host).get)
      .withHttpApp(routes.orNotFound)
      .build
}
