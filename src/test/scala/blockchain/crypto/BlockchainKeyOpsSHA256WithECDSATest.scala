package blockchain.crypto

import cats.effect._
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF

class BlockchainKeyOpsSHA256WithECDSATest extends CatsEffectSuite with ScalaCheckEffectSuite {
  type F[A] = IO[A]

  test("Signing checking") {
    PropF.forAllF { data: Array[Byte] =>
      for {
        pair     <- BlockchainKeyOpsSHA256WithECDSA.generate.pure[F]
        signed   <- BlockchainKeyOpsSHA256WithECDSA.sign(pair.privateKey, data).pure[F]
        verified <- BlockchainKeyOpsSHA256WithECDSA.verify(pair.publicKey, data, signed).pure[F]
        _ = assert(verified)
      } yield ()
    }
  }

  test("Signing checking of incorrect signature") {
    PropF.forAllF { data: Array[Byte] =>
      for {
        nonEmptyData <- data.appended(Byte.MaxValue).pure[F]
        pair         <- BlockchainKeyOpsSHA256WithECDSA.generate.pure[F]
        signed       <- BlockchainKeyOpsSHA256WithECDSA.sign(pair.privateKey, nonEmptyData).pure[F]
        verified     <- BlockchainKeyOpsSHA256WithECDSA.verify(pair.publicKey, nonEmptyData, signed + "!").pure[F]
        _ = assert(!verified)
      } yield ()
    }
  }
}
