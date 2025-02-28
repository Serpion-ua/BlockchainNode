package blockchain.state

import blockchain.model.BlockId
import cats.effect.std.Semaphore
import cats.effect.{Async, Ref}
import cats.implicits._

//Base class which allows to get some STATE on some block id
//Uses BlocksTree for getting right path and perform apply events
//(could be extended to support unapply events for forks)
trait BlockTreeState[F[_], State] {
  def useStateAt[U](id: BlockId)(f: State => F[U]): F[U]
}

object BlockTreeState {
  def make[F[_]: Async, State](
      initialBlockId: F[BlockId],
      initialState: F[State],
      applyEvent: (BlockId, State) => F[State],
      blocksTree: BlocksTree[F]
  ): F[BlockTreeState[F, State]] =
    for {
      semaphore       <- Semaphore[F](1)
      currentBlockId  <- initialBlockId.flatMap(Ref.of[F, BlockId])
      currentStateRef <- initialState.flatMap(Ref.of[F, State])
    } yield new BlockTreeStateImp(semaphore, currentBlockId, currentStateRef, applyEvent, blocksTree)
}

private class BlockTreeStateImp[F[_]: Async, State](
    semaphore: Semaphore[F],
    currentBlockIdRef: Ref[F, BlockId],
    currentStateRef: Ref[F, State],
    applyEvent: (BlockId, State) => F[State],
    blocksTree: BlocksTree[F]
) extends BlockTreeState[F, State] {
  override def useStateAt[U](targetBlockId: BlockId)(f: State => F[U]): F[U] =
    semaphore.permit.use(_ =>
      for {
        currentBlockId <- currentBlockIdRef.get
        state          <- if (currentBlockId == targetBlockId) currentStateRef.get else updateTo(currentBlockId, targetBlockId)
        res            <- f(state)
      } yield res
    )

  private def updateTo(currentBlockId: BlockId, targetBlockId: BlockId): F[State] = {
    for {
      applyChain   <- blocksTree.buildPath(currentBlockId, targetBlockId)
      currentState <- currentStateRef.get
      newState     <- applyEvents(applyChain, currentState)
    } yield newState
  }

  private def applyEvents(eventIds: Seq[BlockId], currentState: State): F[State] =
    eventIds.foldLeftM(currentState) { case (state, blockId) =>
      Async[F].uncancelable(_ =>
        for {
          newState <- applyEvent(blockId, state)
          _        <- (currentStateRef.set(newState), currentBlockIdRef.set(blockId)).tupled
        } yield newState
      )
    }
}
