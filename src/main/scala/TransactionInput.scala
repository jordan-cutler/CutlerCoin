case class TransactionInput(transactionOutputId: String) {
  val UTXO: TransactionOutput = CutlerChain.UTXOs(transactionOutputId)
}
