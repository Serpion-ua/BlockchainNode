package blockchain

import blockchain.crypto.BlockchainKeyOpsSHA256WithECDSA
import blockchain.crypto.BlockchainKeys.BlockchainKeyPair
import blockchain.model.{AccountData, AccountNonce, BlockId}
import blockchain.model.Transaction.CoinAmount
import org.scalacheck.{Arbitrary, Gen}

object Generators {
  implicit val KeyGenerator: Gen[BlockchainKeyPair] = {
    Gen.delay(BlockchainKeyOpsSHA256WithECDSA.generate)
  }
  implicit val ArbitraryKeyGenerator: Arbitrary[BlockchainKeyPair] = Arbitrary(KeyGenerator)

  implicit val CoinAmountGenerator: Gen[CoinAmount] = Gen.long.filter(_ > 0).map(CoinAmount)
  implicit val CoinAmountArbitrary: Arbitrary[CoinAmount] = Arbitrary(CoinAmountGenerator)

  implicit val NonceGenerator: Gen[AccountNonce] = Gen.long.filter(_ > 0).map(AccountNonce.apply)
  implicit val NonceArbitrary: Arbitrary[AccountNonce] = Arbitrary(NonceGenerator)

  implicit val AccountDataGen:Gen[AccountData] =
    for {
      nonce <- Gen.const(AccountNonce(0))
      amount <- CoinAmountGenerator
    } yield AccountData(amount, nonce)

  implicit val AccountDataArbitrary: Arbitrary[AccountData] = Arbitrary(AccountDataGen)

  implicit val BlockIdGenerator: Gen[BlockId] = Gen.long.map(BlockId)
  implicit val BlockIdArbitrary: Arbitrary[BlockId] = Arbitrary(BlockIdGenerator)
}
