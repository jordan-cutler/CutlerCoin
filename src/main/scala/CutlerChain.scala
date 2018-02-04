import java.security.Security

import org.bouncycastle.jce.provider.BouncyCastleProvider

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CutlerChain {

  val MiningDifficulty = 5
  val hashTarget: String = "0" * MiningDifficulty
  val MinimumTransaction = 0.1d
  val UTXOs: mutable.Map[String, TransactionOutput] = new mutable.HashMap[String, TransactionOutput]()
  var genesisTransaction: Transaction = _
  private val blockChain: ArrayBuffer[Block] = mutable.ArrayBuffer[Block]()


  def main(args: Array[String]): Unit = {
    //Setup Bouncey castle as a Security Provider
    Security.addProvider(new BouncyCastleProvider())

    val cutlerWallet = Wallet()
    val coinbase = Wallet()

    genesisTransaction = Transaction(
      sender = coinbase.publicKey,
      recipient = cutlerWallet.publicKey,
      amount = 500000,
      signature = Transaction.generateSignature(
        privateKey = coinbase.privateKey,
        sender = coinbase.publicKey,
        recipient = cutlerWallet.publicKey,
        amount = 500000
      ),
      null
    )
    genesisTransaction.transactionHash = "0"
    genesisTransaction.outputs.append(
      TransactionOutput(
        recipient = genesisTransaction.recipient,
        genesisTransaction.amount,
        genesisTransaction.transactionHash
      )
    )
    CutlerChain.UTXOs.put(genesisTransaction.outputs(0).id, genesisTransaction.outputs(0))

    println("Creating and mining genesis block...")
    val genesisBlock = Block("0")
    genesisBlock.addTransaction(genesisTransaction)
    CutlerChain.insertBlock(genesisBlock)

    val gregWallet = Wallet()
    val block1 = Block(genesisBlock.hash)
    println("\nCutler's Wallet balance is: " + cutlerWallet.getBalance)
    println("\nWalletA is Attempting to send funds (40) to WalletB...")
    block1.addTransaction(
      cutlerWallet.sendFunds(
        recipient = gregWallet.publicKey,
        40
      )
    )
    CutlerChain.insertBlock(block1)
    println("\nCutler's Wallet balance is: " + cutlerWallet.getBalance)
    println("Greg's Wallet balance is: " + gregWallet.getBalance)

    val block2 = Block(block1.hash)
    println("\nCutler is attempting to send more funds (1000) than it has...")
    block2.addTransaction(
      cutlerWallet.sendFunds(
        recipient = gregWallet.publicKey,
        500960
      )
    )
    CutlerChain.insertBlock(block2)
    println("\nCutler's Wallet balance is: " + cutlerWallet.getBalance)
    println("Greg's Wallet balance is: " + gregWallet.getBalance)

    val block3 = Block(block2.hash)
    println("\nGreg is attempting to send funds (20) to Cutler...")
    block3.addTransaction(
      gregWallet.sendFunds(
        recipient = cutlerWallet.publicKey,
        20
      )
    )
    println("\nCutler's Wallet balance is: " + cutlerWallet.getBalance)
    println("Greg's Wallet balance is: " + gregWallet.getBalance)

    if (CutlerChain.isChainValid) {
      println("VALID BLOCKCHAIN")
    } else {
      println("INVALID BLOCKCHAIN")
    }
  }

  def isChainValid: Boolean = {
    for (i <- 1 until blockChain.size) {
      val currentBlock = blockChain(i)
      val previousBlock = blockChain(i - 1)

      val blockIsValid = blockHashEqualsBlockHashCalculation(currentBlock) &&
        previousBlockHashEqualsCurrentBlocksPreviousHash(previousBlock, currentBlock) &&
        blockHasHashTarget(currentBlock) &&
        currentBlock.transactions.forall { transaction =>
          transactionSignatureVerified(transaction) &&
            transactionInputsEqualsOutputs(transaction) &&
            transactionInputsReferencePreviousOutputsCorrectly(transaction) &&
            firstTransactionOutputToTransactionRecipient(transaction) &&
            secondTransactionOutputToSender(transaction)
        }

      if (!blockIsValid) return false
    }
    true
  }

  def blockHashEqualsBlockHashCalculation(block: Block): Boolean = {
    if (block.hash.equals(block.calculateHash())) true
    else {
      println("Current hashes not equal")
      false
    }
  }

  def previousBlockHashEqualsCurrentBlocksPreviousHash(previousBlock: Block, currentBlock: Block): Boolean = {
    if (previousBlock.hash.equals(currentBlock.previousHash)) true
    else {
      println("Previous hashes not equal")
      false
    }
  }

  def blockHasHashTarget(block: Block): Boolean = {
    if (block.hash.substring(0, MiningDifficulty).equals(hashTarget)) true
    else {
      println("This block hasn't been mined")
      false
    }
  }

  def transactionSignatureVerified(transaction: Transaction): Boolean = {
    if (transaction.verifySignature()) true
    else {
      println(s"Signature on transaction(${transaction.transactionHash}) is invalid")
      false
    }
  }

  def transactionInputsEqualsOutputs(transaction: Transaction): Boolean = {
    if (transaction.getInputsValue == transaction.getOutputsValue) true
    else {
      println(s"Inputs are not equal to outputs on transaction(${transaction.transactionHash})")
      false
    }
  }

  def transactionInputsReferencePreviousOutputsCorrectly(transaction: Transaction): Boolean = {
    val mapWithGenesisTransaction = mutable.Map(
      genesisTransaction.outputs(0).id -> genesisTransaction.outputs(0)
    )
    val tempUTXOs =
      transaction.outputs.foldLeft(mapWithGenesisTransaction)(
        (accumulatedMap, currentOutput) => accumulatedMap + (currentOutput.id -> currentOutput)
      )

    transaction.inputs.foreach { input =>
      val outputOpt = tempUTXOs.get(input.transactionOutputId)

      outputOpt match {
        case Some(output) =>
          if (input.UTXO.amount != output.amount) {
            println(s"Referenced unspent transaction from Transaction(${transaction.transactionHash}) input does not equal the amount for the output found with the inputs referenced output hash")
            return false
          }
        case None =>
          println(s"Referenced unspent transaction from Transaction(${transaction.transactionHash}) input is missing")
          return false
      }

      tempUTXOs.remove(input.transactionOutputId)
    }
    true
  }

  def firstTransactionOutputToTransactionRecipient(transaction: Transaction): Boolean = {
    if (transaction.outputs(0).recipient == transaction.recipient) true
    else {
      println(s"Transaction(${transaction.transactionHash}) output recipient is not who it should be")
      false
    }
  }

  def secondTransactionOutputToSender(transaction: Transaction): Boolean = {
    if (transaction.outputs(1).recipient == transaction.sender) true
    else {
      println(s"Transaction(${transaction.transactionHash}) output 'change' is not sender.")
      false
    }
  }


  def insertBlock(block: Block): Unit = {
    block.mineBlock()
    blockChain.append(block)
  }
}
