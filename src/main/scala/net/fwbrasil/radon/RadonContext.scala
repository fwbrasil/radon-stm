package net.fwbrasil.radon

import transaction.TransactionManager
import net.fwbrasil.radon.ref.Ref

trait RadonContext
        extends ref.RefContext
        with transaction.TransactionContext {
    implicit val context = this
    override val milisToWaitBeforeRetry = 1
}

class ConcurrentTransactionException(val refs: List[Ref[_]]) extends Exception
class RetryLimitTransactionException extends Exception
class RequiredTransactionException extends Exception
class NotSupportedTransactionException extends Exception
