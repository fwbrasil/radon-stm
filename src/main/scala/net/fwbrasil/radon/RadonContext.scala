package net.fwbrasil.radon

import transaction.TransactionManager
import net.fwbrasil.radon.ref.Ref

trait RadonContext
		extends ref.RefContext
		with transaction.TransactionContext {
	implicit val context = this
}

class ConcurrentTransactionException(val refs: Ref[_]*) extends Exception {
	def retryWithWrite = false
}
class RetryWithWriteTransactionException(refs: Ref[_]*) extends ConcurrentTransactionException(refs: _*) {
	override def retryWithWrite = true
}
class RetryLimitTransactionException extends Exception
class RequiredTransactionException extends Exception
class NotSupportedTransactionException extends Exception
class InvalidTransactionException extends Exception
class TransactionIsNotThradSafeException extends Exception