import java.security.PublicKey

case class TransactionOutput(
  recipient: PublicKey, // new owner
  amount: Double,
  parentTransactionHash: String // id of the transaction this output was created in
) {
  val id: String = StringUtil.applySha256(
    StringUtil.getStringFromKey(recipient) +
      String.valueOf(amount) +
      parentTransactionHash
  )

  def isMine(publicKey: PublicKey): Boolean = publicKey.equals(recipient)
}
