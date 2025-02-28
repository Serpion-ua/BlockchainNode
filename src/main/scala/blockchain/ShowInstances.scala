package blockchain

import blockchain.crypto.BlockchainKeys.{BlockchainPrivateKey, BlockchainPublicKey}
import blockchain.model.Transaction.{CoinAmount, TransactionId}
import blockchain.model._
import cats.Show
import cats.implicits._

object ShowInstances {
  implicit val publicKeyShow: Show[BlockchainPublicKey] = publicKey => s"${publicKey.value}"

  implicit val privateKeyShow: Show[BlockchainPrivateKey] = privateKey => s"${privateKey.value}"

  implicit val coinAmountShow: Show[CoinAmount] = amount => s"coin amount ${amount.value}"

  implicit val transactionShow: Show[Transaction] = tx => show"Transaction [${tx.id}, from ${tx.from}, to ${tx.to}, ${tx.amount}]"

  implicit val transactionIdShow: Show[TransactionId] = id => show"Transaction id [${id.value}]"

  implicit val nonceShow: Show[AccountNonce] = nonce => show"Nonce [${nonce.value}]"

  implicit val blockShow: Show[Block] = block => show"Block [${block.id}, ${block.header}, ${block.body}]"

  implicit val blockIdShow: Show[BlockId] = blockId => show"Block id [${blockId.value}]"

  implicit val blockHeightShow: Show[BlockHeight] = blockHeight => show"Height [${blockHeight.value}]"

  implicit val blockSignatureShow: Show[BlockSignature] = signature => show"Block signature [${signature.value}]"

  implicit val blockHeaderShow: Show[BlockHeader] = blockHeader =>
    show"Block header  [${blockHeader.height}, parent ${blockHeader.parentId}, txHash ${blockHeader.txsHash}, ${blockHeader.signature}]"

  implicit val unsignedBlockHeaderShow: Show[UnsignedBlockHeader] = blockHeader =>
    show"Block header  [${blockHeader.height}, parent ${blockHeader.parentId}, txHash ${blockHeader.txsHash}]"

  implicit val blockBody: Show[BlockBody] = blockBody => show"Block body: Transactions ${blockBody.txs}"

  implicit val accountShow: Show[Account] = account => show"Account [${account.value}]"

  implicit val accountDataShow: Show[AccountData] = accountData =>
    show"Account Data [${accountData.amount}, ${accountData.lastUsedNonce}]"
}
