package blockchain.ledger

import blockchain.crypto.BlockchainKeyOps
import blockchain.model.Transaction
import blockchain.validator.TransactionVerifier

import scala.annotation.tailrec

//Return sequence of transactions (order is important!) which are compatible with current ledger data
object TransactionsPruner {
  //TODO improve algorithm here! O(N^2) is absolutely inefficient
  def pruneInvalidTxs(txs: Seq[Transaction], ledgerData: LedgerData, crypto: BlockchainKeyOps): Seq[Transaction] = {
    @tailrec
    def loop(remaining: Set[Transaction], acc: List[Transaction], currentLedgerData: LedgerData): List[Transaction] = {
      val (validTxs, invalidTxs) =
        remaining.partition(tx => TransactionVerifier.verifyTransaction(tx, currentLedgerData, crypto).isRight)

      if (validTxs.isEmpty) {
        acc
      } else {
        val newLedgerData = validTxs.foldLeft(currentLedgerData)((ld, tx) => ld.applyTransaction(tx))
        loop(invalidTxs, acc ++ validTxs, newLedgerData)
      }
    }

    loop(txs.toSet, List.empty, ledgerData)
  }
}
