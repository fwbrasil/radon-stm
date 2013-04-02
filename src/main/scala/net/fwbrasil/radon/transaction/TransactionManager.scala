package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.RetryLimitTransactionException
import net.fwbrasil.radon.RadonContext
import net.fwbrasil.radon.RequiredTransactionException
import net.fwbrasil.radon.RetryWithWriteTransactionException
import net.fwbrasil.radon.ConcurrentTransactionException
import net.fwbrasil.radon.util.ExclusiveThreadLocal
import net.fwbrasil.radon.util.Debug
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class TransactionManager(implicit val context: TransactionContext) {

    private[this] val activeTransactionThreadLocal =
        new ExclusiveThreadLocal[Transaction]

    private[radon] def isActive(transaction: Option[Transaction]) =
        getActiveTransaction != None && getActiveTransaction == transaction

    private[radon] def activate(transaction: Option[Transaction]) = {
        val active = getActiveTransaction
        if ((getActiveTransaction != None || transaction == None)
            && getActiveTransaction != transaction)
            throw new IllegalStateException("Another transaction is active.")
        activeTransactionThreadLocal.set(transaction)
    }

    private[radon] def deactivate(transaction: Option[Transaction]) = {
        val active = getActiveTransaction
        if (active != transaction)
            throw new IllegalStateException("Transaction is not active.")
        activeTransactionThreadLocal.clean(transaction)
    }

    private[fwbrasil] def getRequiredActiveTransaction: Transaction = {
        val active = getActiveTransaction
        if (active != None)
            active.get
        else
            throw new RequiredTransactionException
    }

    private[fwbrasil] def getActiveTransaction =
        activeTransactionThreadLocal.get

    private[radon] def runInTransaction[A](transaction: Transaction)(f: => A): A = {
        val someTransaction = Some(transaction)
        activate(someTransaction)
        val res =
            try
                f
            catch {
                case e: Throwable => {
                    deactivate(Option(transaction))
                    transaction.rollback
                    throw e
                }
            }
        deactivate(someTransaction)
        res
    }

    private[radon] def runInNewTransactionWithRetry[A](f: => A): A =
        runInTransactionWithRetry(new Transaction, f)

    protected def waitToRetry(e: ConcurrentTransactionException) =
        if (context.milisToWaitBeforeRetry > 0)
            Thread.sleep(context.milisToWaitBeforeRetry)

    @tailrec private[radon] final def runInTransactionWithRetry[A](
        transaction: Transaction, f: => A, retryCount: Int = 0): A = {
        if (retryCount >= context.retryLimit)
            throw new RetryLimitTransactionException
        Try {
            val result = runInTransaction(transaction)(f)
            transaction.commit
            result
        } match {
            case Success(result) =>
                result
            case Failure(e: ConcurrentTransactionException) =>
                waitToRetry(e)
                transaction.isRetryWithWrite = e.retryWithWrite
                runInTransactionWithRetry(transaction, f, retryCount + 1)
            case Failure(other) =>
                throw other
        }
    }

}