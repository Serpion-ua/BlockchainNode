package blockchain

import blockchain.Genesis.GenesisHeight
import blockchain.ShowInstances._
import blockchain.crypto.BlockchainKeyOps
import blockchain.crypto.BlockchainKeys.{BlockchainKeyPair, BlockchainPrivateKey}
import blockchain.crypto.Signable.{SignableSyntax, TransactionSigning, UnsignedBlockSigning}
import blockchain.model.Transaction.CoinAmount
import blockchain.model._
import cats.implicits._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import scala.annotation.tailrec

case class BlocksContext(lastBlockHeader: BlockHeader)

case class ChainGeneratorState(
    ledger: Map[BlockchainKeyPair, AccountData],
    blocksContext: BlocksContext,
    generatorSeed: Seed,
    parameters: Gen.Parameters
)

//TODO BlockchainKeyOps.generate accept seed and generate keys based on that seed, to get the same generated data
object ChainGenerator {
  private val desiredTotalLedgers = 20
  private val maxTxInBlock        = 5

  def generateTransaction(state: ChainGeneratorState)(implicit crypto: BlockchainKeyOps): (Transaction, ChainGeneratorState) = {
    val rnd                = new MutableSeed(state.generatorSeed)
    val nonEmptyKeys       = IndexedSeq.from(state.ledger.filter(_._2.amount.value > 0).keys).sortBy(_.publicKey.hashCode())
    val nonEmptyKeysAmount = nonEmptyKeys.length

    val (from, to) = if (nonEmptyKeys.length > desiredTotalLedgers) {
      (nonEmptyKeys(rnd.nextPositiveInt % nonEmptyKeysAmount), nonEmptyKeys(rnd.nextPositiveInt % nonEmptyKeysAmount))
    } else {
      //TODO: we shall reuse currently empty Keys as well
      (nonEmptyKeys(rnd.nextPositiveInt % nonEmptyKeysAmount), crypto.generate)
    }

    val fromData                = state.ledger(from)
    val fromAmount: CoinAmount  = fromData.amount
    val fromNonce: AccountNonce = fromData.lastUsedNonce.nextNonce

    val toData                     = state.ledger.getOrElse(to, AccountData.default)
    val toAmount: CoinAmount       = toData.amount
    val transferAmount: CoinAmount = if (rnd.nextBool) fromAmount else rnd.nextCoinAmount % fromAmount

    val tx =
      Transaction(from.publicKey, to.publicKey, fromNonce, transferAmount, TransactionSignature.empty).sign(from.privateKey)

    val newFromData       = AccountData(fromAmount - transferAmount, fromNonce)
    val newToData         = AccountData(toAmount + transferAmount, toData.lastUsedNonce)
    val newLedger         = state.ledger ++ Map(from -> newFromData, to -> newToData)
    val newChainGenerator = state.copy(ledger = newLedger, generatorSeed = rnd.seed)
    (tx, newChainGenerator)
  }

  @tailrec
  private def txsGenerationIteration(
      acc: Seq[Transaction],
      state: ChainGeneratorState,
      iteration: Int
  )(implicit crypto: BlockchainKeyOps): (Seq[Transaction], ChainGeneratorState) = {
    if (iteration > 0) {
      val (newTx, newChainGenerator) = generateTransaction(state)
      txsGenerationIteration(acc :+ newTx, newChainGenerator, iteration - 1)
    } else {
      (acc, state)
    }
  }

  def generateBlock(state: ChainGeneratorState, nodeKeys: BlockchainKeyPair)(implicit
      crypto: BlockchainKeyOps
  ): (Block, ChainGeneratorState) = {
    val rnd             = new MutableSeed(state.generatorSeed)
    val txAmount        = rnd.nextPositiveInt % maxTxInBlock
    val (txs, newState) = txsGenerationIteration(Seq.empty[Transaction], state.copy(generatorSeed = rnd.seed), txAmount)

    val blockBody   = BlockBody(txs)
    val blockHeight = state.blocksContext.lastBlockHeader.height.childHeight
    val parentId    = state.blocksContext.lastBlockHeader.id
    val txsHash     = blockBody.txsHash
    val blockHeader = UnsignedBlockHeader(blockHeight, parentId, txsHash).sign(nodeKeys.privateKey)
    (Block(blockHeader, blockBody), newState.copy(blocksContext = state.blocksContext.copy(lastBlockHeader = blockHeader)))
  }

  @tailrec
  private def blockGenerationIteration(
      acc: Seq[Block],
      state: ChainGeneratorState,
      iteration: Int,
      nodeKeys: BlockchainKeyPair
  )(implicit crypto: BlockchainKeyOps): (Seq[Block], ChainGeneratorState) = {
    if (iteration > 0) {
      val (newBlock, newChainGenerator) = generateBlock(state, nodeKeys)
      blockGenerationIteration(acc :+ newBlock, newChainGenerator, iteration - 1, nodeKeys)
    } else {
      (acc, state)
    }
  }

  def generateBlocks(state: ChainGeneratorState, amount: Int, nodeKeys: BlockchainKeyPair)(implicit
      crypto: BlockchainKeyOps
  ): (Seq[Block], ChainGeneratorState) = {
    blockGenerationIteration(Seq.empty[Block], state, amount, nodeKeys)
  }
}

object ChainGeneratorState {
  val genesisParentId: BlockId = BlockId(0)

  //TODO genesis transactions shall use different addresses
  def fromGenesisBlock(genesisBlock: Block, privateKeys: Seq[BlockchainPrivateKey], seed: Seed = Seed.random()): ChainGeneratorState = {
    val ledger = genesisBlock.body.txs.zip(privateKeys).map { case (tx, privateKey) =>
      val blockchainKeyPair = BlockchainKeyPair(privateKey, tx.to)
      val accountData = AccountData.default.copy(amount = tx.amount)
      blockchainKeyPair -> accountData
    }.toMap

    val blocksContext = BlocksContext(genesisBlock.header)
    new ChainGeneratorState(ledger, blocksContext, seed, Gen.Parameters.default)
  }

  def fromKeysAndAmounts(data: Seq[(BlockchainKeyPair, AccountData)], nodeKeys: BlockchainKeyPair, seed: Seed = Seed.random())(
      implicit crypto: BlockchainKeyOps
  ): (Block, ChainGeneratorState) = {
    val ledger = data.toMap
    val genesisTxs: Seq[Transaction] = data.map { case (keyPair, data) =>
      Transaction(Account.dead.value, keyPair.publicKey, AccountNonce.initialNonce, data.amount, TransactionSignature.empty)
    }
    val genesisBlockBody: BlockBody = BlockBody(genesisTxs)
    val genesisUnsignedHeader = UnsignedBlockHeader(GenesisHeight, genesisParentId, genesisBlockBody.txsHash)
    val genesisHeader               = genesisUnsignedHeader.sign(nodeKeys.privateKey)
    val genesisBlock: Block         = Block(genesisHeader, genesisBlockBody)

    val blocksContext = BlocksContext(genesisHeader)
    (genesisBlock, new ChainGeneratorState(ledger, blocksContext, seed, Gen.Parameters.default))
  }

}

class MutableSeed(var seed: Seed) {
  private def safeAbs(value: Long): Long = {
    if (value == Long.MinValue) Long.MaxValue // Avoid overflow
    else Math.abs(value)
  }

  def nextLong: Long = {
    val (res, newSeed) = seed.long
    seed = newSeed
    res
  }

  def nextCoinAmount: CoinAmount = {
    val value = safeAbs(nextLong) % 100
    CoinAmount(value)
  }

  def nextPositiveInt: Int = {
    val (res, newSeed) = seed.long
    seed = newSeed
    (safeAbs(res) % Int.MaxValue).toInt
  }

  def nextBool: Boolean = {
    val (res, newSeed) = seed.long
    seed = newSeed
    (res % 2) == 0
  }
}
