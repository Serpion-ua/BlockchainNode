package blockchain

import blockchain.model.BlockId
import cats.effect.implicits._
import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import fs2.Stream
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import blockchain.ShowInstances._

//Contains information about current local chain state,
//could be extended for comparing different chains (f.e. some remote chain)
object LocalChain {
  def make[F[_]: Async: Logger](currentHead: BlockId): Resource[F, LocalChain[F]] = {
    for {
      semaphore      <- Semaphore[F](1).toResource
      headRef        <- Ref.of(currentHead).toResource
      adoptionsTopic <- Resource.make(Topic[F, BlockId])(_.close.void)
    } yield new LocalChain[F] {
      override def adopt(blockId: BlockId): F[Unit] = {
        semaphore.permit.use { _ =>
          Sync[F].uncancelable { _ =>
            for {
              _ <- headRef.set(blockId)
              _ <- adoptionsTopic.publish1(blockId)
              - <- Logger[F].info(show"Fully adopted $blockId")
            } yield ()
          }
        }
      }

      override def adoptions: F[Stream[F, BlockId]] = Async[F].delay(adoptionsTopic.subscribeUnbounded)

      override def head: F[BlockId] = headRef.get
    }
  }
}

trait LocalChain[F[_]] {
  def adopt(block: BlockId): F[Unit]

  def adoptions: F[Stream[F, BlockId]]

  def head: F[BlockId]
}
