package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.{ NotSupportedTransactionException, RequiredTransactionException }

trait Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A
}

/**
 * If the transaction attribute is Required, the container ensures that the enterprise bean's method will always be invoked with a JTA transaction.
 * If the calling client is associated with a JTA transaction, the enterprise bean method will be invoked in the same transaction context.
 * However, if a client is not associated with a transaction, the container will automatically begin a new transaction and try to commit the
 * transaction when the method completes.
 */
class Required extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
        import context.transactionManager._
        if (transaction != None) {
            val wasActive = isActive(transaction)
            try {
                runInTransaction(transaction.get)(f)
            } finally
                if (wasActive)
                    activate(transaction)
        } else
            runInNewTransactionWithRetry(f)
    }
}

/**
 * The transaction attribute Mandatory requires the container to invoke a bean's method in a client's transaction context.
 * If the client is not associated with a transaction context when calling this method, the container throws
 * javax.transaction.TransactionRequiredException if the client is a remote client or javax.ejb.TransactionRequiredLocalException
 * if the client is a local client. If the calling client has a transaction context, the case is treated as Required by the container.
 */
class Mandatory extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
        import context.transactionManager._
        if (transaction == None)
            throw new RequiredTransactionException
        val wasActive = isActive(transaction)
        try {
            runInTransaction(transaction.get)(f)
        } finally
            if (wasActive)
                activate(transaction)
    }
}

/**
 * The transaction attribute Never requires that the enterprise bean method explicitly not be called within a transaction context.
 * If the client calls with a transaction context, the container throws java.rmi.RemoteException if the client is a remote
 * client or javax.ejb.EJBException if the client is a local client. If the client is not associated with any transaction context,
 * the container invokes the method without initiating a transaction.
 */
class Never extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
        import context.transactionManager._
        if (transaction != None || getActiveTransaction != None)
            throw new NotSupportedTransactionException
        f
    }
}

/**
 * If the transaction attribute is NotSupported, the transactional context of the calling client is not propagated to the enterprise bean.
 * If a client calls with a transaction context, the container suspends the client's transaction association before invoking the
 * enterprise bean's method. After the method completes, the container resumes the suspended transaction association.
 */
class NotSupported extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
        import context.transactionManager._
        val wasActive = isActive(transaction)
        if (wasActive)
            deactivate(transaction)
        try {
            f
        } finally
            if (wasActive)
                activate(transaction)
    }
}

/**
 * If the transaction attribute is RequiresNew, the container always creates a new transaction before invoking the enterprise bean
 * method and commits the transaction when the method returns. If the calling client is associated with a transaction context, the
 * container suspends the association of the transaction context with the current thread before starting the new transaction.
 * When the method and the transaction complete, the container resumes the suspended transaction.
 */
class RequiresNew extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
        import context.transactionManager._
        val wasActive = isActive(transaction)
        if (wasActive)
            deactivate(transaction)
        try {
            runInNewTransactionWithRetry(f)
        } finally
            if (wasActive)
                activate(transaction)
    }
}

/**
 * It the transaction attribute is Supports and the client is associated with a transaction context, the context is propagated
 * to the enterprise bean method, similar to the way the container treats the Required case. If the client call is not
 * associated with any transaction context, the container behaves similarly to the NotSupported case. The transaction
 * context is not propagated to the enterprise bean method.
 */
class Supports extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
        import context.transactionManager._
        if (transaction != None)
            runInTransaction(transaction.get)(f)
        else
            f
    }
}
class Transient extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit ctx: TransactionContext): A = {
        import ctx.transactionManager._
        val wasActive = isActive(transaction)
        if (wasActive)
            deactivate(transaction)
        try {
            runInTransactionWithRetry(new Transaction(transient = true)(ctx), f)
        } finally
            if (wasActive)
                activate(transaction)
    }
}
trait PropagationContext {
    type Propagation = net.fwbrasil.radon.transaction.Propagation
    val required = new net.fwbrasil.radon.transaction.Required
    val mandatory = new net.fwbrasil.radon.transaction.Mandatory
    val never = new net.fwbrasil.radon.transaction.Never
    val notSupported = new net.fwbrasil.radon.transaction.NotSupported
    val requiresNew = new net.fwbrasil.radon.transaction.RequiresNew
    val supports = new net.fwbrasil.radon.transaction.Supports
    val nested = new net.fwbrasil.radon.transaction.Nested
    val transient = new net.fwbrasil.radon.transaction.Transient
}