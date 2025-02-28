package blockchain

import blockchain.crypto.BlockchainKeyOpsSHA256WithECDSA
import blockchain.crypto.BlockchainKeys.{BlockchainKeyPair, BlockchainPrivateKey, BlockchainPublicKey}
import blockchain.crypto.Signable.{SignableSyntax, TransactionSigning}
import blockchain.model.Transaction.CoinAmount
import blockchain.model._

object Genesis {
  val GenesisParentId: BlockId = BlockId(0)

  val GenesisHeight: BlockHeight = BlockHeight(0)

  //Obviously, private key here only because it is POC, private key shall be removed,
  //or it could be used for private testnet only
  val GenesisKey1: BlockchainKeyPair =
    BlockchainKeyPair(
      BlockchainPrivateKey(
        "MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgKMejvxcCtjgd4TVVOpGqcX/C4E19GqM6xZJLH897pD+gCgYIKoZIzj0DAQehRANCAAROO9a9Pj6DZEagNZyo2nfYMkG163fA7TV246JE0ZLKCilH6rsm+BBGELrfAbf6l5LqZqvkM4tVt4w3QRBS4NNO"
      ),
      BlockchainPublicKey(
        "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAETjvWvT4+g2RGoDWcqNp32DJBtet3wO01duOiRNGSygopR+q7JvgQRhC63wG3+peS6mar5DOLVbeMN0EQUuDTTg=="
      )
    )
  val GenesisAmount: CoinAmount = CoinAmount(1000000)
  //Genesis transaction is not signed
  val GenesisTx: Transaction =
    Transaction(Account.dead.value, GenesisKey1.publicKey, AccountNonce.initialNonce, GenesisAmount, TransactionSignature.empty)
  val GenesisBlockBody: BlockBody = BlockBody(Seq(GenesisTx))
  private val unsignedGenesisBlockHeader: BlockHeader =
    BlockHeader(GenesisHeight, GenesisParentId, GenesisBlockBody.txsHash, BlockSignature.empty)
  private val genesisBlockHeaderSignature: BlockSignature = BlockSignature(
    BlockchainKeyOpsSHA256WithECDSA.sign(GenesisKey1.privateKey, unsignedGenesisBlockHeader.toSign)
  )
  val GenesisHeader: BlockHeader = unsignedGenesisBlockHeader.copy(signature = genesisBlockHeaderSignature)
  val GenesisBlock: Block        = Block(GenesisHeader, GenesisBlockBody)
  val GenesisBlockId: BlockId    = GenesisBlock.id
}
