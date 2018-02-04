import java.security.{PrivateKey, PublicKey}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Transaction(
  sender: PublicKey,
  recipient: PublicKey,
  amount: Double,
  signature: Array[Byte],
  inputs: List[TransactionInput]
) {
  var transactionHash: String = calculateTransactionHash()
  val outputs: ArrayBuffer[TransactionOutput] = mutable.ArrayBuffer[TransactionOutput]()

  def calculateTransactionHash(): String = {
    Transaction.transactionCounter.incrementCounter
    StringUtil.applySha256(
      StringUtil.getStringFromKey(sender) +
        StringUtil.getStringFromKey(recipient) +
        String.valueOf(amount) +
        Transaction.transactionCounter.getTransactionCount
    )
  }

  def verifySignature(): Boolean = {
    val data = StringUtil.getStringFromKey(sender) + StringUtil.getStringFromKey(recipient) + String.valueOf(amount)
    StringUtil.verifyECDSASig(sender, data, signature)
  }

  def processTransaction(): Boolean = {
    if (!verifySignature()) {
      println("Transaction signature failed to verify")
      return false
    }
    val inputsValue = getInputsValue
    if (inputsValue < CutlerChain.MinimumTransaction) {
      println("Transaction Inputs too small: " + inputsValue)
      return false
    }
    def generateTransactionOutputs(): Unit = {
      val leftOver = inputsValue - amount
      outputs.append(
        TransactionOutput(recipient, amount, transactionHash),
        TransactionOutput(sender, leftOver, transactionHash)
      )
    }

    def addOutputsToUnspentList(): Unit = {
      outputs.foreach(output => CutlerChain.UTXOs.put(output.id, output))
    }

    def removeInputsFromUnspentList(): Unit = {
      inputs.foreach(input => CutlerChain.UTXOs.remove(input.transactionOutputId))
    }

    generateTransactionOutputs()
    addOutputsToUnspentList()
    removeInputsFromUnspentList()

    true
  }

  def getInputsValue: Double = {
    inputs.foldLeft(0d)((acc, current) => acc + current.UTXO.amount)
  }

  def getOutputsValue: Double = {
    outputs.foldLeft(0d)((acc, current) => acc + current.amount)
  }
}

object Transaction {
  val transactionCounter = TransactionCounter()

  def generateSignature(privateKey: PrivateKey, sender: PublicKey, recipient: PublicKey, amount: Double): Array[Byte] = {
    val data = StringUtil.getStringFromKey(sender) + StringUtil.getStringFromKey(recipient) + String.valueOf(amount)
    StringUtil.applyECDSASig(privateKey, data)
  }
}
