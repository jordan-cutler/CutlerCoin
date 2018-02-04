import java.util.Date

import scala.collection.mutable.ArrayBuffer

case class Block(previousHash: String) {
  private var nonce: Int = 0
  val timeCreated: Long = new Date().getTime
  var hash: String = ""
  var merkleRoot: String = ""
  val transactions: ArrayBuffer[Transaction] = new ArrayBuffer[Transaction]()

  def calculateHash(): String = {
    StringUtil.applySha256(
      previousHash +
        String.valueOf(timeCreated) +
        Integer.toString(nonce) +
        merkleRoot
    )
  }

  def mineBlock(): String = {
    val target = "0" * CutlerChain.MiningDifficulty
    do {
      nonce = nonce + 1
      hash = calculateHash()
    } while (!hash.substring(0, CutlerChain.MiningDifficulty).equals(target))
    println("Block Mined!!! : " + hash)
    hash
  }

  def addTransaction(transaction: Transaction): Boolean = {
    if (transaction == null) return false
    if (!previousHash.equals("0") && !transaction.processTransaction()) {
      println("Transaction failed to process. Discarded")
      return false
    }
    transactions.append(transaction)
    println("Transaction successfully added to block")
    true
  }
}
