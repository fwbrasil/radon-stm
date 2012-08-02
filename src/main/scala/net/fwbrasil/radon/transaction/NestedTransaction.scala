package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.RequiredTransactionException
import net.fwbrasil.radon.RadonContext
import net.fwbrasil.radon.ref.Ref

final class NestedTransaction(val parent: Transaction)(override implicit val context: TransactionContext)
		extends Transaction()(context) {

	startTimestamp = parent.startTimestamp
	endTimestamp = parent.endTimestamp

	override def get[T](ref: Ref[T]): Option[T] = {
		if (refsWrite.contains(ref))
			super.get(ref)
		else
			parent.get(ref)
	}

	override def commit(): Unit = {
		parent.refsRead ++= refsRead
		parent.refsWrite ++= refsWrite
		parent.refsSnapshot.putAll(refsSnapshot)
		clear
	}

	override private[radon] def isDestroyed[T](ref: Ref[T]): Boolean = {
		super.isDestroyed(ref) || parent.isDestroyed(ref)
	}

	override private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
		super.isDirty(ref) || parent.isDirty(ref)
	}
}

class Nested extends Propagation {
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
		import context.transactionManager._
		if (transaction == None)
			throw new RequiredTransactionException
		deactivate(transaction)
		try {
			val nested = new NestedTransaction(transaction.get)
			runInTransactionWithRetry(nested)(f)
		} finally
			activate(transaction)
	}
}
