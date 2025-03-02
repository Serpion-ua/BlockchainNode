package blockchain.crypto

import blockchain.crypto.BlockchainKeys.{BlockchainKeyPair, BlockchainPrivateKey, BlockchainPublicKey}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.util.encoders.Base64

import java.security._
import java.security.spec.{ECGenParameterSpec, PKCS8EncodedKeySpec, X509EncodedKeySpec}
import scala.util.Try

object BlockchainKeyOpsSHA256WithECDSA extends BlockchainKeyOps {
  private val emptyString = ""

  Security.addProvider(new BouncyCastleProvider())
  private val keyGen: KeyPairGenerator = KeyPairGenerator.getInstance("EC", "BC")
  keyGen.initialize(new ECGenParameterSpec("secp256r1"))

  override def sign(secret: BlockchainPrivateKey, messageBytes: Array[Byte]): String = {
    val secretKey            = secretFromString(secret.value)
    val signature: Signature = Signature.getInstance("SHA256withECDSA", "BC")
    if (messageBytes.isEmpty) {
      emptyString
    } else {
      signature.initSign(secretKey)
      signature.update(messageBytes)
      bytesToString(signature.sign())
    }
  }

  override def verify(public: BlockchainPublicKey, messageBytes: Array[Byte], signedMessageString: String): Boolean = {
    Try {
      if (messageBytes.isEmpty) {
        true
      } else {
        val publicKey            = publicFromString(public.value)
        val signature: Signature = Signature.getInstance("SHA256withECDSA", "BC")
        signature.initVerify(publicKey)
        signature.update(messageBytes)
        signature.verify(stringToBytes(signedMessageString))
      }
    }.getOrElse(false)
  }

  override def generate: BlockchainKeyPair = this.synchronized {
    val keys = keyGen.generateKeyPair()
    BlockchainKeyPair(secretToString(keys.getPrivate), publicToString(keys.getPublic))
  }

  override def isValidPublicKey(public: BlockchainPublicKey): Boolean =
    Try {
      publicFromString(public.value)
    }.isSuccess

  override def isValidPrivateKey(secret: BlockchainPrivateKey): Boolean =
    Try {
      secretFromString(secret.value)
    }.isSuccess


  private def bytesToString(bytes: Array[Byte]) = Base64.toBase64String(bytes)
  private def stringToBytes(string: String)     = Base64.decode(string)

  private def secretToString(secret: java.security.PrivateKey): BlockchainPrivateKey =
    BlockchainPrivateKey(bytesToString(secret.getEncoded))

  private def secretFromString(secretString: String): java.security.PrivateKey = {
    val keyFactory = KeyFactory.getInstance("EC", "BC")
    val keySpec    = new PKCS8EncodedKeySpec(stringToBytes(secretString))
    keyFactory.generatePrivate(keySpec)
  }

  private def publicToString(public: java.security.PublicKey): BlockchainPublicKey =
    BlockchainPublicKey(bytesToString(public.getEncoded))

  private def publicFromString(publicString: String): java.security.PublicKey = {
    val keyFactory = KeyFactory.getInstance("EC", "BC")
    val keySpec    = new X509EncodedKeySpec(stringToBytes(publicString))
    keyFactory.generatePublic(keySpec)
  }
}
