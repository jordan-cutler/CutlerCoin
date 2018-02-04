import java.security._
import java.util.Base64

object StringUtil {
  def applySha256(input: String): String = {
    try {
      val digest: MessageDigest = MessageDigest.getInstance("SHA-256")
      //Applies sha256 to our input
      val hash: Array[Byte] = digest.digest(input.getBytes("UTF-8"))
      hash.foldLeft(new StringBuffer())((hexString, byte) => {
        val hex = Integer.toHexString(0xff & byte)
        if (hex.length() == 1) hexString.append('0')
        hexString.append(hex)
      }).toString
    }
    catch {
      case e: Exception => throw new RuntimeException(e)
    }
  }

  def applyECDSASig(privateKey: PrivateKey, input: String): Array[Byte] = {
    try {
      val dsa: Signature = Signature.getInstance("ECDSA", "BC")
      dsa.initSign(privateKey)
      dsa.update(input.getBytes)
      dsa.sign()
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
  }

  def verifyECDSASig(publicKey: PublicKey, data: String, signature: Array[Byte]): Boolean = {
    try {
      val ecdsaVerify: Signature = Signature.getInstance("ECDSA", "BC")
      ecdsaVerify.initVerify(publicKey)
      ecdsaVerify.update(data.getBytes)
      ecdsaVerify.verify(signature)
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
  }

  def getStringFromKey(key: Key): String = {
    Base64.getEncoder.encodeToString(key.getEncoded)
  }

  def getMerkleRoot(transactions: List[Transaction]): String = {
    var previousTreeLayer: IndexedSeq[String] = transactions.map(_.transactionHash).toIndexedSeq

    do {
      val treeLayer =
        for (i <- 1 until previousTreeLayer.size)
          yield applySha256(previousTreeLayer(i - 1) + applySha256(previousTreeLayer(i)))

      previousTreeLayer = treeLayer
    } while (previousTreeLayer.size > 1)

    if (previousTreeLayer.size == 1) previousTreeLayer.head
    else ""
  }
}
