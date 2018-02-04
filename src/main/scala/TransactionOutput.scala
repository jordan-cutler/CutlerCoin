import java.security.PublicKey

case class TransactionOutput(
  recipient: PublicKey,
  amount: Double,
  transactionHashOutputWasCreatedIn: String
) {
  val id: String = StringUtil.applySha256(
    StringUtil.getStringFromKey(recipient) +
      String.valueOf(amount) +
      transactionHashOutputWasCreatedIn
  )

  def isMine(publicKey: PublicKey): Boolean = publicKey.equals(recipient)
}
