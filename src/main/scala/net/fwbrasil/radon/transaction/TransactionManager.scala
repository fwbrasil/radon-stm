package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.RetryLimitTransactionException
import net.fwbrasil.radon.RadonContext
import net.fwbrasil.radon.RequiredTransactionException
import net.fwbrasil.radon.ConcurrentTransactionException
import net.fwbrasil.radon.util.ExclusiveThreadLocal
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class TransactionManager(implicit val context: TransactionContext) {

    private[this] val activeTransactionThreadLocal =
        new ExclusiveThreadLocal[Transaction]

    private[radon] def isActive(transaction: Option[Transaction]) =
        getActiveTransaction.isDefined && getActiveTransaction == transaction

    private[radon] def activate(transaction: Option[Transaction]) = {
        val active = getActiveTransaction
        if ((getActiveTransaction.isDefined || transaction.isEmpty)
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
        if (active.isDefined)
            active.get
        else
            throw new RequiredTransactionException
    }

    private[fwbrasil] def getActiveTransaction =
        activeTransactionThreadLocal.get

    private[radon] def runInTransaction[A](transaction: Transaction)(f: => A): A = {
        val someTransaction = Some(transaction)
        activate(someTransaction)
        try
            f
        catch {
            case e: Throwable =>
                throw e
        } finally
            deactivate(someTransaction)
    }

    private[radon] def runInNewTransactionWithRetry[A](typ: TransactionType, f: => A): A =
        runInTransactionWithRetry(new Transaction(false, typ), f)

    @tailrec private[radon] final def runInTransactionWithRetry[A](
        transaction: Transaction, f: => A, retryCount: Int = 0): A = {
        if (retryCount >= context.retryLimit)
            throw new RetryLimitTransactionException
        try {
            val result = runInTransaction(transaction)(f)
            transaction.commit
            result
        } catch {
            case e: ConcurrentTransactionException =>
                transaction.rollback
                waitToRetry(e)
                runInTransactionWithRetry(transaction, f, retryCount + 1)
            case other: Throwable =>
                transaction.rollback
                throw other
        }
    }

    private[radon] def runInTransactionWithRetryAsync[A](
        future: => Future[A],
        ctx: TransactionalExecutionContext,
        retryCount: Int = 0): Future[A] = {
        implicit val ectx = ctx.ctx.ectx
        future
            .flatMap {
                result =>
                    ctx.transaction.asyncCommit.map(_ => result)
            }.recoverWith {
                case e: ConcurrentTransactionException =>
                    if (retryCount >= context.retryLimit)
                        throw new RetryLimitTransactionException
                    else {
                        waitToRetry(e)
                        runInTransactionWithRetryAsync(future, ctx, retryCount + 1)
                    }
            }
    }

    protected def waitToRetry(e: ConcurrentTransactionException) =
        if (context.milisToWaitBeforeRetry > 0)
            Thread.sleep(context.milisToWaitBeforeRetry)

}