package blockchain.crypto

import cats.effect._
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF

class BlockchainKeyOpsRSATest extends CatsEffectSuite with ScalaCheckEffectSuite {
  type F[A] = IO[A]

  test("Signing checking") {
    PropF.forAllF { data: Array[Byte] =>
      for {
        pair     <- BlockchainKeyOpsRSA.generate.pure[F]
        signed1  <- BlockchainKeyOpsRSA.sign(pair.privateKey, data).pure[F]
        signed2  <- BlockchainKeyOpsRSA.sign(pair.privateKey, data).pure[F]
        verified <- BlockchainKeyOpsRSA.verify(pair.publicKey, data, signed1).pure[F]
        _ = assert(verified)
        _ = assert(signed1 == signed2)
      } yield ()
    }
  }

  test("Signing checking of incorrect signature") {
    PropF.forAllF { data: Array[Byte] =>
      for {
        nonEmptyData <- data.appended(Byte.MaxValue).pure[F]
        pair         <- BlockchainKeyOpsRSA.generate.pure[F]
        signed       <- BlockchainKeyOpsRSA.sign(pair.privateKey, nonEmptyData).pure[F]
        verified     <- BlockchainKeyOpsRSA.verify(pair.publicKey, nonEmptyData, signed + "!").pure[F]
        _ = assert(!verified)
      } yield ()
    }
  }
}
