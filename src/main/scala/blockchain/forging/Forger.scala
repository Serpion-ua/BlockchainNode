package blockchain.forging

import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeyOps
import blockchain.crypto.BlockchainKeys.BlockchainPrivateKey
import blockchain.crypto.Signable._
import blockchain.model.{Block, UnsignedBlock}
import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import fs2.Stream
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger

trait Forger[F[_]] {
  def forgeBlock(unsignedBlock: UnsignedBlock): F[Block]
  def forgedBlocksStream: F[Stream[F, Block]]
}

object Forger {
  def make[F[_]: Async: Logger](signingKey: BlockchainPrivateKey)(implicit crypto: BlockchainKeyOps): Resource[F, Forger[F]] = {
    for {
      forgedBlocks <- Resource.make(Topic[F, Block])(_.close.void)
    } yield new Forger[F] {
      override def forgeBlock(unsignedBlock: UnsignedBlock): F[Block] =
        for {
          block <- signBlock(unsignedBlock)
          _     <- Logger[F].info(show"Forge block ${block.id} with parent ${block.header.parentId}")
          _ <- forgedBlocks
            .publish1(block)
            .map(_.leftMap(_ => new IllegalStateException("Forging topic is closed")))
            .rethrow
        } yield block

      private def signBlock(unsignedBlock: UnsignedBlock): F[Block] = {
        Logger[F].info(show"Going to sign block ${unsignedBlock.unsignedHeader} with key $signingKey") >>
          Block(unsignedBlock.unsignedHeader.sign(signingKey), unsignedBlock.body).pure[F]
      }

      override def forgedBlocksStream: F[Stream[F, Block]] = Sync[F].delay(forgedBlocks.subscribeUnbounded)
    }
  }
}
