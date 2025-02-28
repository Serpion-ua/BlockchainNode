package blockchain.ledger

import blockchain.model.{Account, AccountData, Transaction}

class LedgerData(val accountToData: Map[Account, AccountData]) {
  //We do NOT know any verification rules here, we just apply block to the ledger data
  //thus we even could get non exist account data with zero balance as source of coins or other incorrect state,
  //or apply transaction with incorrect nonce as well
  //However, such transactions shall be declined previously on the verification stage
  def applyTransaction(tx: Transaction): LedgerData = {
    val fromAccount     = Account(tx.from)
    val fromData        = accountToData.getOrElse(fromAccount, AccountData.default)
    val newNonce        = if (tx.nonce > fromData.lastUsedNonce) tx.nonce else fromData.lastUsedNonce
    val updatedFromData = AccountData(fromData.amount - tx.amount, newNonce)

    val toAccount     = Account(tx.to)
    val toData        = accountToData.getOrElse(toAccount, AccountData.default)
    val updatedToData = AccountData(toData.amount + tx.amount, toData.lastUsedNonce)

    val newAccountToData =
      accountToData ++
        Map(fromAccount -> updatedFromData, toAccount -> updatedToData).filter { case (account, _) => account != Account.dead }
    new LedgerData(newAccountToData)
  }

}

object LedgerData {
  val empty: LedgerData = new LedgerData(Map.empty[Account, AccountData])
}
