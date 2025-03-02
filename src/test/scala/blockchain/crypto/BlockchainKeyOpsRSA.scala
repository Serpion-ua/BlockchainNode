package blockchain.crypto

import blockchain.crypto.BlockchainKeys.{BlockchainKeyPair, BlockchainPrivateKey, BlockchainPublicKey}
import org.bouncycastle.util.encoders.Base64

import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security._
import scala.util.Try

object BlockchainKeyOpsRSA extends BlockchainKeyOps {
  private val keyGen: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
  keyGen.initialize(512, new SecureRandom()) // Very insecure but fast

  override def sign(secret: BlockchainPrivateKey, messageBytes: Array[Byte]): String = {
    if (messageBytes.isEmpty) "" else Base64.toBase64String(messageBytes)
  }

  override def verify(public: BlockchainPublicKey, messageBytes: Array[Byte], signedMessageString: String): Boolean = {
    Try {
      if (messageBytes.isEmpty) true
      else messageBytes.sameElements(Base64.decode(signedMessageString))
    }.getOrElse(false)
  }

  override def generate: BlockchainKeyPair = {
    val keyPair = keyGen.generateKeyPair()
    BlockchainKeyPair(
      BlockchainPrivateKey(Base64.toBase64String(keyPair.getPrivate.getEncoded)),
      BlockchainPublicKey(Base64.toBase64String(keyPair.getPublic.getEncoded))
    )
  }

  override def isValidPublicKey(public: BlockchainPublicKey): Boolean = {
    Try {
      val keySpec    = new X509EncodedKeySpec(Base64.decode(public.value))
      val keyFactory = KeyFactory.getInstance("RSA")
      keyFactory.generatePublic(keySpec).isInstanceOf[PublicKey]
    }.getOrElse(false)
  }

  def isValidPrivateKey(secret: BlockchainPrivateKey): Boolean = {
    Try {
      val keySpec    = new PKCS8EncodedKeySpec(Base64.decode(secret.value))
      val keyFactory = KeyFactory.getInstance("RSA")
      keyFactory.generatePrivate(keySpec).isInstanceOf[PrivateKey]
    }.getOrElse(false)
  }
}
