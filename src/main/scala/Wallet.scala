import java.security._
import java.security.spec.ECGenParameterSpec

case class Wallet() {
  private val keyPair: KeyPair = generateKeyPair()

  val privateKey: PrivateKey = keyPair.getPrivate
  val publicKey: PublicKey = keyPair.getPublic

  def generateKeyPair(): KeyPair = {
    try {
      val keyGen = KeyPairGenerator.getInstance("ECDSA", "BC")
      val random = SecureRandom.getInstance("SHA1PRNG")
      val ecSpec = new ECGenParameterSpec("prime192v1")
      // Initialize the key generator and generate a KeyPair
      keyGen.initialize(ecSpec, random) //256 bytes provides an acceptable security level
      keyGen.generateKeyPair()
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
  }

  def getBalance: Double = getBalanceAndUnspentTransactions.balance

  def sendFunds(recipient: PublicKey, amount: Double): Transaction = {
    val balanceWithUnspentTransactions = getBalanceAndUnspentTransactions
    if (balanceWithUnspentTransactions.balance < amount) {
      println("Not enough funds to send transaction. Transaction discarded.")
      return null
    }

    def generateTransactionInputs(): List[TransactionInput] = {
      balanceWithUnspentTransactions.unspentTransactions.values.foldLeft((0d, List[TransactionInput]()))(
        (accum, transactionOutput) => {
          if (accum._1 > amount) return accum._2
          (accum._1 + transactionOutput.amount, TransactionInput(transactionOutput.id) :: accum._2)
        }
      )
    }._2

    val inputs: List[TransactionInput] = generateTransactionInputs()

    Transaction(
      sender = publicKey,
      recipient = recipient,
      amount,
      Transaction.generateSignature(
        privateKey, publicKey, recipient, amount
      ),
      inputs
    )
  }

  private def getBalanceAndUnspentTransactions: BalanceWithUnspentTransactions = {
    CutlerChain.UTXOs.keys.foldLeft(BalanceWithUnspentTransactions(0d, Map[String, TransactionOutput]()))(
      (accum: BalanceWithUnspentTransactions, unspentTransactionHash: String) =>
        CutlerChain.UTXOs.get(unspentTransactionHash).map { output =>
          if (output.isMine(publicKey)) {
            BalanceWithUnspentTransactions(accum.balance + output.amount, accum.unspentTransactions + (unspentTransactionHash -> output))
          }
          else accum
        }.getOrElse(accum)
    )
  }
}

case class BalanceWithUnspentTransactions(balance: Double, unspentTransactions: Map[String, TransactionOutput])

