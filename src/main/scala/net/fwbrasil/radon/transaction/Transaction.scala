
package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.ConcurrentTransactionException
import net.fwbrasil.radon.RadonContext
import scala.collection.mutable.HashSet
import net.fwbrasil.radon.ref.Ref
import net.fwbrasil.radon.ref.RefContent
import net.fwbrasil.radon.util.Statistics
import net.fwbrasil.radon.util.ExclusiveThreadLocalItem
import net.fwbrasil.radon.util.Debug
import net.fwbrasil.radon.util.Lockable._
import net.fwbrasil.radon.transaction.time._
import java.util.IdentityHashMap

class IdentityHashSet[A] extends HashSet[A] {
	override def elemHashCode(key: A) = java.lang.System.identityHashCode(key)
}

trait TransactionImplicits {

	private[transaction] implicit def toAnyRef[T](ref: Ref[T]) =
		ref.asInstanceOf[Ref[Any]]

	private[transaction] implicit def toAnyRefContent[T](refContent: RefContent[T]) =
		refContent.asInstanceOf[RefContent[Any]]
}

trait RefSnapshooter {
	this: TransactionImplicits with TransactionStopWatch =>

	private[transaction] var refsSnapshot = new IdentityHashMap[Ref[Any], RefContent[Any]]()

	private[transaction] def hasAValidSnapshot[T](ref: Ref[T]) =
		refsSnapshot.get(ref).writeTimestamp < startTimestamp

	private[transaction] def hasAnOutdatedSnapshot[T](ref: Ref[T]) = {
		val snapshot = refsSnapshot.get(ref)
		(snapshot.writeTimestamp != ref.refContent.writeTimestamp
			&& snapshot.readTimestamp != ref.refContent.readTimestamp)
	}

	private[transaction] def hasDestroyedFlag[T](ref: Ref[T]) =
		ref.refContent.destroyedFlag || {
			val snapshotOption = Option(refsSnapshot.get(ref))
			(snapshotOption.isDefined && snapshotOption.get.destroyedFlag)
		}

	private[transaction] def snapshot[T](ref: Ref[T]) =
		if (!refsSnapshot.containsKey(ref))
			refsSnapshot.put(toAnyRef(ref), ref.refContent)

	private[transaction] def snapshot[T](ref: Ref[T], detroyed: Boolean): Unit = {
		val oldContent =
			Option(refsSnapshot.get(ref)).getOrElse(ref.refContent)
		val newContents = RefContent(oldContent.value, oldContent.readTimestamp, oldContent.writeTimestamp, detroyed)
		refsSnapshot.put(toAnyRef(ref), newContents)
	}

	private[transaction] def snapshot[T](ref: Ref[T], value: Option[T]): Unit = {
		val oldContent =
			Option(refsSnapshot.get(ref)).getOrElse(ref.refContent)
		val newContents = RefContent(value, oldContent.readTimestamp, oldContent.writeTimestamp, false)
		refsSnapshot.put(toAnyRef(ref), newContents)
	}

	private[transaction] def getSnapshot[T](ref: Ref[T]) =
		refsSnapshot.get(ref)

	private[transaction] def clearSnapshots =
		refsSnapshot = new IdentityHashMap[Ref[Any], RefContent[Any]]()
}

trait TransactionValidator {
	this: Transaction =>

	private[transaction] def validateWrite[T](ref: Ref[T]) =
		retryIfTrue(isRefRedAfterTheStartOfTransaction(ref) || isRefDestroyedAfterTheStartOfTransaction(ref), ref)

	private[transaction] def validateRead[T](ref: Ref[T]) =
		retryIfTrue((isRefWroteAfterTheStartOfTransaction(ref) || hasAnOutdatedSnapshot(ref)) && !(isReadOnly && hasAValidSnapshot(ref)), ref)

	private[transaction] def validateIfIsDestroyed[T](ref: Ref[T]) =
		if (hasDestroyedFlag(ref))
			throw new IllegalStateException("Trying to access a destroyed ref.")

	private[transaction] def validateContext[T](ref: Ref[T]) =
		if (ref.context != context)
			throw new IllegalStateException("Ref is from another context!")

	private[transaction] def validateConcurrentRefCreation[T](ref: Ref[T]) =
		retryIfTrue(isRefCreatingInAnotherTransaction(ref), ref)

	private[this] def isRefRedAfterTheStartOfTransaction[T](ref: Ref[T]) =
		ref.refContent.readTimestamp > startTimestamp

	private[this] def isRefCreatingInAnotherTransaction[T](ref: Ref[T]) =
		ref.isCreating && ref.creationTransaction != this

	private[this] def isRefWroteAfterTheStartOfTransaction[T](ref: Ref[T]) =
		ref.refContent.writeTimestamp > startTimestamp

	private[this] def isRefDestroyedAfterTheStartOfTransaction[T](ref: Ref[T]) =
		isRefWroteAfterTheStartOfTransaction(ref) && ref.destroyedFlag
}

class Transaction(val transient: Boolean)(implicit val context: TransactionContext)
		extends TransactionImplicits
		with TransactionStopWatch
		with RefSnapshooter
		with TransactionValidator
		with ExclusiveThreadLocalItem {

	def this()(implicit context: TransactionContext) = this(false)

	import context._

	private[radon] var isRetryWithWrite = false
	private[transaction] var refsRead = new IdentityHashSet[Ref[Any]]()
	private[transaction] var refsWrite = new IdentityHashSet[Ref[Any]]()

	def refsAssignments =
		(for (refWrite <- refsWrite.toList)
			yield (refWrite -> {
			val snapshot = getSnapshot(refWrite)
			(snapshot.value, snapshot.destroyedFlag)
		})).toList

	private[transaction] def isReadOnly =
		!isRetryWithWrite && refsWrite.isEmpty

	private[this] def isWrite =
		refsWrite.nonEmpty

	private[this] def isWriteOnly =
		isWrite && refsRead.isEmpty

	private[this] def isTransactionDoNothing =
		refsWrite.isEmpty && refsRead.isEmpty

	private[transaction] def retryIfTrue(condition: Boolean, refs: Ref[_]*) =
		if (condition)
			retry(refs: _*)

	private[radon] def put[T](ref: Ref[T], value: Option[T]) = {
		startIfNotStarted
		validateIfIsDestroyed(ref)
		snapshot(ref, value)
		refsWrite += ref
	}

	private[radon] def get[T](ref: Ref[T]): Option[T] = {
		startIfNotStarted
		validateIfIsDestroyed(ref)
		snapshot(ref)
		refsRead += ref
		getSnapshot(ref).value.asInstanceOf[Option[T]]
	}

	private[radon] def destroy[T](ref: Ref[T]): Unit = {
		startIfNotStarted
		validateIfIsDestroyed(ref)
		snapshot(ref, true)
		refsWrite += ref
	}

	private[radon] def isDestroyed[T](ref: Ref[T]): Boolean = {
		startIfNotStarted
		hasDestroyedFlag(ref)
	}

	private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
		startIfNotStarted
		refsWrite.contains(ref)
	}

	def commit(): Unit = {
		if (!isTransactionDoNothing) try {

			val refsReadWithoutWrite = refsRead -- refsWrite
			val readUnlockeds = lockall(refsReadWithoutWrite, _.tryReadLock)
			try {
				retryIfTrue(readUnlockeds.nonEmpty, readUnlockeds.toSeq: _*)
				val writeUnlockeds = lockall(refsWrite, _.tryWriteLock)

				try {
					retryIfTrue(writeUnlockeds.nonEmpty, writeUnlockeds.toSeq: _*)

					stop

					refsRead.foreach(validateContext(_))
					refsWrite.foreach(validateContext(_))

					refsRead.foreach(validateConcurrentRefCreation(_))
					refsWrite.foreach(validateConcurrentRefCreation(_))

					refsRead.foreach(validateRead(_))
					refsWrite.foreach(validateWrite(_))

					try
						if (!transient && isWrite)
							context.makeDurable(this)
					catch {
						case e =>
							prepareRollback
							throw e
					} finally {

						for (ref <- refsReadWithoutWrite)
							setRefContent(ref, false, true)

						for (ref <- refsWrite)
							setRefContent(ref, true, refsRead.contains(ref))

						Statistics.commitCount.increment
					}
				} finally {
					val writeLockeds = (refsWrite -- writeUnlockeds)
					writeLockeds.foreach(_.writeUnlock)
					writeLockeds.foreach((ref) => ref.synchronized(ref.notify))
				}
			} finally {
				val readLockeds = (refsReadWithoutWrite -- readUnlockeds)
				readLockeds.foreach(_.readUnlock)
				readLockeds.foreach((ref) => ref.synchronized(ref.notify))
			}
		} finally
			clear
	}

	private[this] def setRefContent(ref: Ref[_], isRefWrite: Boolean, isRefRead: Boolean) = ref.synchronized {
		val refContent = ref.refContent
		val (value, destroyedFlag) =
			if (!transient && isRefWrite && refContent.writeTimestamp < startTimestamp) {
				val snapshot = getSnapshot(ref)
				(snapshot.value, snapshot.destroyedFlag)
			} else
				(refContent.value, refContent.destroyedFlag)
		val readTimestamp =
			if (!transient && isRefRead && !isReadOnly && refContent.readTimestamp < startTimestamp)
				startTimestamp
			else
				refContent.readTimestamp
		val writeTimestamp =
			if (!transient && isRefWrite && refContent.writeTimestamp < startTimestamp)
				endTimestamp
			else
				refContent.writeTimestamp
		ref.setRefContent(value, readTimestamp, writeTimestamp, destroyedFlag)
	}

	def prepareRollback = {
		val refsWrote = refsWrite
		val refsCreated =
			refsWrote.filter(_.creationTransaction == this)
		clearValues
		for (ref <- refsCreated)
			destroy(ref)
		for (ref <- refsWrote)
			ref.notifyRollback
	}

	def rollback() = {
		prepareRollback
		commit
	}

	private[this] def clearValues = {
		refsRead = new IdentityHashSet[Ref[Any]]()
		refsWrite = new IdentityHashSet[Ref[Any]]()
		clearSnapshots
	}

	private[transaction] def clear = {
		clearValues
		clearStopWatch
	}

}

trait TransactionContext extends PropagationContext {

	private[radon] val transactionManager =
		new TransactionManager()(this)

	private[radon] val transactionClock = new time.TransactionClock

	val retryLimit = 3000

	type Transaction = net.fwbrasil.radon.transaction.Transaction

	def transactional[A](f: => A): A =
		transactional(transactionManager.getActiveTransaction, required)(f)

	def transactional[A](propagation: net.fwbrasil.radon.transaction.Propagation)(f: => A): A =
		transactional(transactionManager.getActiveTransaction, propagation)(f)

	def transactional[A](pTransaction: net.fwbrasil.radon.transaction.Transaction)(f: => A): A =
		transactional(Option(pTransaction))(f)

	def transactional[A](pTransaction: Option[net.fwbrasil.radon.transaction.Transaction])(f: => A): A =
		transactional(pTransaction, required)(f)

	def transactional[A](pTransaction: net.fwbrasil.radon.transaction.Transaction, propagation: Propagation)(f: => A): A =
		transactional(Option(pTransaction), propagation)(f)

	def transactional[A](transaction: Option[net.fwbrasil.radon.transaction.Transaction], propagation: Propagation)(f: => A): A = {
		val activeTransaction = transactionManager.getActiveTransaction
		if (activeTransaction != None && activeTransaction != transaction)
			throw new IllegalStateException("There is another transaction active!")
		if (transaction.isDefined)
			transaction.synchronized {
				propagation.execute(transaction)(f)(this)
			}
		else
			propagation.execute(transaction)(f)(this)
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
	def retry(refs: Ref[_]*) =
		throw new ConcurrentTransactionException(refs: _*)

	def makeDurable(transaction: Transaction) = {

	}
}