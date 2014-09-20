package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.ConcurrentTransactionException
import net.fwbrasil.radon.ref.Ref
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import net.fwbrasil.radon.RetryLimitTransactionException
import java.util.concurrent.Executors

trait TransactionContext extends PropagationContext {

    protected[fwbrasil] val transactionManager =
        new TransactionManager()(this)

    private[radon] val transactionClock = new time.TransactionClock

    val retryLimit = 3000
    val milisToWaitBeforeRetry = 1

    val executionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

    type ReadOnly = net.fwbrasil.radon.transaction.ReadOnly
    type ReadWrite = net.fwbrasil.radon.transaction.ReadWrite

    val readOnly = net.fwbrasil.radon.transaction.ReadOnly()
    val readWrite = net.fwbrasil.radon.transaction.ReadWrite()

    private[fwbrasil] implicit val ectx = executionContext

    type Transaction = net.fwbrasil.radon.transaction.Transaction

    def transactional[A](f: => A): A =
        transactional(transactionManager.getActiveTransaction, required, readWrite)(f)

    def transactional[A](typ: TransactionType = readWrite)(f: => A): A =
        transactional(transactionManager.getActiveTransaction, required, typ)(f)

    def transactional[A](propagation: net.fwbrasil.radon.transaction.Propagation)(f: => A): A =
        transactional(transactionManager.getActiveTransaction, propagation, readWrite)(f)

    def transactional[A](typ: TransactionType, propagation: net.fwbrasil.radon.transaction.Propagation)(f: => A): A =
        transactional(transactionManager.getActiveTransaction, propagation, typ)(f)

    def transactional[A](pTransaction: net.fwbrasil.radon.transaction.Transaction)(f: => A): A =
        transactional(Option(pTransaction))(f)

    def transactional[A](pTransaction: Option[net.fwbrasil.radon.transaction.Transaction])(f: => A): A =
        transactional(pTransaction, required, readWrite)(f)

    def transactional[A](pTransaction: net.fwbrasil.radon.transaction.Transaction, propagation: Propagation)(f: => A): A =
        transactional(Option(pTransaction), propagation, readWrite)(f)

    private def transactional[A](transaction: Option[net.fwbrasil.radon.transaction.Transaction], propagation: Propagation, typ: TransactionType)(f: => A): A = {
        val activeTransaction = transactionManager.getActiveTransaction
        if (activeTransaction.isDefined && activeTransaction != transaction)
            throw new IllegalStateException("There is another active transaction!")
        if (transaction.isDefined)
            propagation.execute(transaction, typ)(f)(this)
        else
            propagation.execute(transaction, typ)(f)(this)
    }

    def asyncTransactional[A](f: => A): Future[A] =
        asyncTransactionalChain(readWrite)(Future(f)(_))

    def asyncTransactional[A](typ: TransactionType)(f: => A): Future[A] =
        asyncTransactionalChain(typ)(Future(f)(_))

    def asyncTransactionalChain[A](fFuture: TransactionalExecutionContext => Future[A]): Future[A] =
        asyncTransactionalChain(readWrite)(fFuture)

    def asyncTransactionalChain[A](typ: TransactionType)(fFuture: TransactionalExecutionContext => Future[A]) = {
        val ctx = new TransactionalExecutionContext(typ)(this)
        transactionManager.runInTransactionWithRetryAsync(fFuture(ctx), ctx)
    }

    def transactionalWhile[A](cond: => Boolean)(f: => A): Unit = {
        var continue: Boolean = true
        while (continue) transactional {
            continue = cond
            if (continue) {
                f
            }
        }
    }

    def retry(refs: Ref[_]*): Unit =
        retry(refs.toList)

    def retry(refs: List[Ref[_]]): Unit =
        throw new ConcurrentTransactionException(refs)

    def makeDurableAsync(transaction: Transaction)(implicit ectx: ExecutionContext): Future[Unit] =
        Future()

    def beforeCommit(transaction: Transaction) = {}
    def afterCommit(transaction: Transaction) = {}

    def makeDurable(transaction: Transaction) = {}

}

class TransactionalExecutionContext(typ: TransactionType = ReadWrite())(implicit val ctx: TransactionContext) extends ExecutionContext {
    val transaction = new Transaction(transactionType = typ)
    override def execute(runnable: Runnable): Unit =
        ctx.ectx.execute {
            new Runnable {
                override def run =
                    transactional {
                        runnable.run
                    }
            }
        }
    def transactional[R](f: => R) =
        transaction.synchronized {
            ctx.transactional(transaction)(f)
        }
    override def reportFailure(t: Throwable): Unit =
        ctx.ectx.reportFailure(t)
}