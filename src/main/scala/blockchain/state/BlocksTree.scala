package blockchain.state

import blockchain.ShowInstances._
import blockchain.model.BlockId
import cats.effect.{Async, Ref}
import cats.implicits._
import org.typelevel.log4cats.Logger

//"Tree" which have information about ALL child-parent relations (shall be modified for supporting forks)
object BlocksTree {
  def make[F[_]: Async: Logger](initialTree: IndexedSeq[BlockId]): F[BlocksTree[F]] = {
    for {
      initial <- Ref.of(initialTree)
    } yield new BlocksTree(initial)
  }
}


class BlocksTree[F[_]: Async: Logger] private (tree: Ref[F, IndexedSeq[BlockId]]) {
  //Support only linear structure, for fork support actual tree graph shall be used
  def parentOf(parent: BlockId, child: BlockId): F[Unit] = {
    tree.update{state =>
      if (state.last == parent)
        state.appended(child)
      else
        throw new IllegalStateException(s"Fork are not supported for now, tried to fork: $parent - $child")
    }
  }

  def buildPath(from: BlockId, to: BlockId): F[Seq[BlockId]] = {
    tree.get.map{tree =>
      val startIndex = tree.indexOf(from)
      val endIndex = tree.indexOf(to)

      if (startIndex == -1) throw new NoSuchElementException(show"BlockId $from not found in BlocksTree")
      if (endIndex == -1) throw new NoSuchElementException(show"BlockId $to not found in BlocksTree")
      if (startIndex > endIndex) throw new IllegalStateException(s"Can't build path $from - $to")

      tree.slice(startIndex + 1, endIndex + 1)
    }
  }
}
