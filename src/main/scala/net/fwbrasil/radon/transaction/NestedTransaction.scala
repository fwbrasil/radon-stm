package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.RequiredTransactionException
import net.fwbrasil.radon.RadonContext
import net.fwbrasil.radon.ref.Ref
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final class NestedTransaction(val parent: Transaction, transactionType: TransactionType = readWrite)(override implicit val context: TransactionContext)
        extends Transaction(false, transactionType)(context) {

    startTimestamp = parent.startTimestamp
    endTimestamp = parent.endTimestamp

    override def get[T](ref: Ref[T]): Option[T] = {
        if (super.isDirty(ref))
            super.get(ref)
        else
            parent.get(ref)
    }

    override def commit(): Unit = {
        import scala.collection.JavaConversions._
        val parentSnaps = parent.refsSnapshot
        for ((ref, snap) <- refsSnapshot) {
            val parentSnap = parentSnaps.get(ref)
            if (parentSnap != null) {
                parentSnap.destroyedFlag |= snap.destroyedFlag
                parentSnap.isRead |= snap.isRead
                parentSnap.isWrite |= snap.isWrite
                if (snap.isWrite)
                    parentSnap.value = snap.value
            } else
                parentSnaps.put(ref, snap)
        }
        clear
    }

    override def asyncCommit()(implicit ectx: ExecutionContext) =
        Future.successful(commit())

    private[radon] def rootTransaction: Transaction =
        parent match {
            case nested: NestedTransaction =>
                nested.rootTransaction
            case normal =>
                normal
        }

    override private[radon] def isDestroyed[T](ref: Ref[T]): Boolean = {
        super.isDestroyed(ref) || parent.isDestroyed(ref)
    }

    override private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
        super.isDirty(ref) || parent.isDirty(ref)
    }
}

class Nested extends Propagation {
    private[transaction] def execute[A](transaction: Option[Transaction], transactionType: TransactionType)(f: => A)(implicit ctx: TransactionContext): A = {
        import ctx.transactionManager._
        if (transaction.isEmpty)
            throw new RequiredTransactionException
        deactivate(transaction)
        try {
            val nested = new NestedTransaction(transaction.get, transactionType)(ctx)
            runInTransactionWithRetry(nested, f)
        } finally
            activate(transaction)
    }
}
