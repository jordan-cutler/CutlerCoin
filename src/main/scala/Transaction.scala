import java.security.{PrivateKey, PublicKey}

import Transaction._

case class Transaction(
  sender: PublicKey,
  recipient: PublicKey,
  amount: Double,
  signature: Array[Byte],
  inputs: List[TransactionInput]
) {

  var transactionHash: String = calculateTransactionHash()
  private val inputsValue = if (inputs != null) getInputsValue else 0d
  var outputs: List[TransactionOutput] = generateTransactionOutputs(inputsValue, amount)

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
    if (inputsValue < CutlerChain.MinimumTransaction) {
      println("Transaction Inputs too small: " + inputsValue)
      return false
    }

    addOutputsToUnspentList(outputs)
    removeInputsFromUnspentList(inputs)

    true
  }

  def getInputsValue: Double = {
    inputs.foldLeft(0d)((acc, current) => acc + current.UTXO.amount)
  }

  def getOutputsValue: Double = {
    outputs.foldLeft(0d)((acc, current) => acc + current.amount)
  }

  private def generateTransactionOutputs(inputsValue: Double, amount: Double): List[TransactionOutput] = {
    val leftOver = inputsValue - amount
    List(
      TransactionOutput(
        recipient = recipient,
        amount = amount,
        transactionHashOutputWasCreatedIn = transactionHash
      ),
      TransactionOutput(
        recipient = sender,
        amount = leftOver,
        transactionHashOutputWasCreatedIn = transactionHash
      )
    )
  }
}

object Transaction {
  val transactionCounter = TransactionCounter()

  def generateSignature(privateKey: PrivateKey, sender: PublicKey, recipient: PublicKey, amount: Double): Array[Byte] = {
    val data = StringUtil.getStringFromKey(sender) + StringUtil.getStringFromKey(recipient) + String.valueOf(amount)
    StringUtil.applyECDSASig(privateKey, data)
  }

  private def addOutputsToUnspentList(outputs: List[TransactionOutput]): Unit = {
    outputs.foreach(output => CutlerChain.UTXOs.put(output.id, output))
  }

  private def removeInputsFromUnspentList(inputs: List[TransactionInput]): Unit = {
    inputs.foreach(input => CutlerChain.UTXOs.remove(input.transactionOutputId))
  }
}
