package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.{ NotSupportedTransactionException, RequiredTransactionException }

trait Propagation {
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A
}

class Required private[radon] extends Propagation {
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
class Mandatory extends Propagation {
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
		import context.transactionManager._
		if (transaction == None)
			throw new RequiredTransactionException
		runInTransaction(transaction.get)(f)
	}
}
class Never extends Propagation {
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
		import context.transactionManager._
		if (transaction != None || getActiveTransaction != None)
			throw new NotSupportedTransactionException
		f
	}
}
class NotSupported extends Propagation {
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
		import context.transactionManager._
		val wasActive = isActive(transaction)
		if(wasActive)
			deactivate(transaction)
		try {
			f
		} finally
			if (wasActive)
				activate(transaction)
	}
}
class RequiresNew extends Propagation {
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
		import context.transactionManager._
		val wasActive = isActive(transaction)
		if(wasActive)
			deactivate(transaction)
		try {
			runInNewTransactionWithRetry(f)
		} finally
			if (wasActive)
				activate(transaction)
	}
}
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
	private[transaction] def execute[A](transaction: Option[Transaction])(f: => A)(implicit context: TransactionContext): A = {
		import context.transactionManager._
		val wasActive = isActive(transaction)
		if(wasActive)
			deactivate(transaction)
		try {
			runInTransactionWithRetry(new Transaction(transient = true))(f)
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