case class TransactionCounter() {
  private var counter: BigInt = BigInt(0)

  def getTransactionCount: Int = counter.intValue()

  def incrementCounter: Int = {
    counter = counter + 1
    getTransactionCount
  }
}
