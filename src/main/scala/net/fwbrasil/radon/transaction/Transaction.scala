
package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.ConcurrentTransactionException
import net.fwbrasil.radon.RadonContext
import scala.collection.mutable.HashSet
import net.fwbrasil.radon.ref.Ref
import net.fwbrasil.radon.ref.RefContent
import net.fwbrasil.radon.util.ExclusiveThreadLocalItem
import net.fwbrasil.radon.util.Debug
import net.fwbrasil.radon.util.Lockable._
import net.fwbrasil.radon.transaction.time._
import java.util.IdentityHashMap
import java.util.HashMap
import java.util.{ HashSet => JHashSet }

trait RefSnapshooter {
	this: Transaction =>

	private[transaction] var refsSnapshot = new IdentityHashMap[Ref[Any], RefContent[Any]]()

	private[transaction] def isAValidSnapshot(snapshot: RefContent[Any]) =
		snapshot.writeTimestamp < startTimestamp

	private[transaction] def isAnOutdatedSnapshot(ref: Ref[Any], snapshot: RefContent[Any]) =
		(snapshot.writeTimestamp != ref.refContent.writeTimestamp
			&& snapshot.readTimestamp != ref.refContent.readTimestamp)

	private[transaction] def hasDestroyedFlag(ref: Ref[Any], snapshot: RefContent[Any]): Boolean =
		ref.refContent.destroyedFlag ||
			(snapshot != null && snapshot.destroyedFlag)

	private[transaction] def snapshot(ref: Ref[Any]) = {
		val snap = refsSnapshot.get(ref)
		validateIfDestroyed(ref, snap)
		if (snap == null) {
			refsSnapshot.put(ref, ref.refContent)
			ref.refContent
		} else
			snap
	}

	private[transaction] def snapshot(ref: Ref[Any], detroyed: Boolean): Unit = {
		val snap = refsSnapshot.get(ref)
		validateIfDestroyed(ref, snap)
		val oldContent =
			Option(snap).getOrElse(ref.refContent)
		val newContents = new RefContent(oldContent.value, oldContent.readTimestamp, oldContent.writeTimestamp, detroyed)
		refsSnapshot.put(ref, newContents)
	}

	private[transaction] def snapshot(ref: Ref[Any], value: Option[Any]): Unit = {
		val snap = refsSnapshot.get(ref)
		validateIfDestroyed(ref, snap)
		val oldContent =
			Option(snap).getOrElse(ref.refContent)
		val newContents = new RefContent(value, oldContent.readTimestamp, oldContent.writeTimestamp, false)
		refsSnapshot.put(ref, newContents)
	}

	private def validateIfDestroyed(ref: Ref[Any], snapshot: RefContent[Any]) =
		if (hasDestroyedFlag(ref, snapshot))
			throw new IllegalStateException("Triyng to access a destroyed ref.")

	private[transaction] def getSnapshot(ref: Ref[Any]) =
		refsSnapshot.get(ref)

	private[transaction] def clearSnapshots =
		refsSnapshot = new IdentityHashMap[Ref[Any], RefContent[Any]]()
}

trait TransactionValidator {
	this: Transaction =>

	private[transaction] def validateWrite(ref: Ref[Any]) =
		retryIfTrue(isRefReadAfterTheStartOfTransaction(ref) || isRefDestroyedAfterTheStartOfTransaction(ref), ref)

	private[transaction] def validateRead(ref: Ref[Any]) = {
		val snapshot = getSnapshot(ref)
		retryIfTrue((isRefWroteAfterTheStartOfTransaction(ref) || isAnOutdatedSnapshot(ref, snapshot)) && !(isReadOnly && isAValidSnapshot(snapshot)), ref)
	}

	private[transaction] def validateContext(ref: Ref[Any]) =
		if (ref.context != context)
			throw new IllegalStateException("Ref is from another context!")

	private[transaction] def validateConcurrentRefCreation(ref: Ref[Any]) =
		retryIfTrue(isRefCreatingInAnotherTransaction(ref), ref)

	private[this] def isRefReadAfterTheStartOfTransaction(ref: Ref[Any]) =
		ref.refContent.readTimestamp > startTimestamp

	private[this] def isRefCreatingInAnotherTransaction(ref: Ref[Any]) =
		ref.creationTransaction != this && ref.isCreating

	private[this] def isRefWroteAfterTheStartOfTransaction(ref: Ref[Any]) =
		ref.refContent.writeTimestamp > startTimestamp

	private[this] def isRefDestroyedAfterTheStartOfTransaction(ref: Ref[Any]) =
		isRefWroteAfterTheStartOfTransaction(ref) && ref.destroyedFlag
}

class Transaction(val transient: Boolean)(implicit val context: TransactionContext)
		extends TransactionStopWatch
		with RefSnapshooter
		with TransactionValidator
		with ExclusiveThreadLocalItem {

	def this()(implicit context: TransactionContext) = this(false)

	import context._

	private[radon] var isRetryWithWrite = false
	private[transaction] var refsRead = new JHashSet[Ref[Any]]()
	private[transaction] var refsWrite = new JHashSet[Ref[Any]]()

	def reads = {
		import scala.collection.JavaConversions._
		refsRead.toSet
	}
	def assignments = {
		import scala.collection.JavaConversions._
		for (refWrite <- refsWrite.toList) yield {
			val snapshot = getSnapshot(refWrite)
			(refWrite, snapshot.value, snapshot.destroyedFlag)
		}
	}

	private[transaction] def isReadOnly =
		!isRetryWithWrite && refsWrite.isEmpty

	private[this] def isWrite =
		!refsWrite.isEmpty

	private[this] def isWriteOnly =
		isWrite && refsRead.isEmpty

	private[transaction] def retryIfTrue(condition: Boolean, refs: Ref[_]*) =
		if (condition)
			retry(refs: _*)

	private[radon] def put[T](ref: Ref[T], value: Option[T]) = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		startIfNotStarted
		snapshot(anyRef, value)
		refsWrite.add(anyRef)
	}

	private[radon] def get[T](ref: Ref[T]): Option[T] = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		startIfNotStarted
		val result = snapshot(anyRef)
		refsRead.add(anyRef)
		result.value.asInstanceOf[Option[T]]
	}

	private[radon] def destroy[T](ref: Ref[T]): Unit = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		startIfNotStarted
		snapshot(anyRef, true)
		refsWrite.add(anyRef)
	}

	private[radon] def isDestroyed[T](ref: Ref[T]): Boolean = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		startIfNotStarted
		hasDestroyedFlag(anyRef, getSnapshot(anyRef))
	}

	private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		startIfNotStarted
		refsWrite.contains(anyRef)
	}

	def commit(): Unit =
		commit(rollback = false)

	private def commit(rollback: Boolean): Unit = {
		import scala.collection.JavaConversions._
		try {
			val refsReadWithoutWrite = refsRead -- refsWrite
			val (readLockeds, readUnlockeds) = lockall(refsReadWithoutWrite, _.tryReadLock)
			try {
				val (writeLockeds, writeUnlockeds) = lockall(refsWrite, _.tryWriteLock)
				try {

					startIfNotStarted
					stop

					try {
						retryIfTrue(readUnlockeds.nonEmpty, readUnlockeds.toSeq: _*)
						retryIfTrue(writeUnlockeds.nonEmpty, writeUnlockeds.toSeq: _*)

						refsReadWithoutWrite.foreach(e => {
							validateContext(e)
							validateConcurrentRefCreation(e)
						})

						refsRead.foreach(validateRead)

						refsWrite.foreach(e => {
							validateContext(e)
							validateConcurrentRefCreation(e)
							validateWrite(e)
						})

						if (!transient && !rollback)
							context.makeDurable(this)
					} catch {
						case e =>
							prepareRollback
							throw e
					} finally {

						for (ref <- refsReadWithoutWrite)
							setRefContent(ref, false, true)

						for (ref <- refsWrite)
							setRefContent(ref, true, refsRead.contains(ref))

					}
				} finally {
					writeLockeds.foreach(_.writeUnlock)
					writeLockeds.foreach((ref) => ref.synchronized(ref.notify))
				}
			} finally {
				readLockeds.foreach(_.readUnlock)
				readLockeds.foreach((ref) => ref.synchronized(ref.notify))
			}
		} finally
			clear
	}

	private[this] def setRefContent(ref: Ref[Any], isRefWrite: Boolean, isRefRead: Boolean) =
		if (!transient) ref.synchronized {
			val refContent = ref.refContent
			val newRefContent =
				valueAndDestroyedFlag(ref, isRefWrite, isRefRead, refContent)
			val read =
				readTimestamp(isRefRead, refContent)
			val write =
				writeTimestamp(isRefWrite, refContent)
			require(ref.creationTransaction != this || write != 0)
			ref.setRefContent(newRefContent.value, read, write, newRefContent.destroyedFlag)
		}

	private[this] def valueAndDestroyedFlag(ref: Ref[Any], isRefWrite: Boolean, isRefRead: Boolean, refContent: RefContent[_]) =
		if (isRefWrite && refContent.writeTimestamp < startTimestamp)
			getSnapshot(ref)
		else
			refContent

	private[this] def readTimestamp(isRefRead: Boolean, refContent: RefContent[_]) =
		if (isRefRead && refContent.readTimestamp < startTimestamp && !isReadOnly)
			startTimestamp
		else
			refContent.readTimestamp

	private[this] def writeTimestamp(isRefWrite: Boolean, refContent: RefContent[_]) =
		if (isRefWrite && refContent.writeTimestamp < startTimestamp)
			endTimestamp
		else
			refContent.writeTimestamp

	def prepareRollback = {
		import scala.collection.JavaConversions._
		val refsWrote = refsWrite
		val refsCreated =
			refsWrote.filter(_.creationTransaction == this)
		clear
		for (ref <- refsCreated)
			destroy(ref)
		for (ref <- refsWrote)
			ref.notifyRollback
	}

	def rollback() = {
		prepareRollback
		commit(rollback = true)
	}

	private[transaction] def clear = {
		refsRead = new JHashSet[Ref[Any]]()
		refsWrite = new JHashSet[Ref[Any]]()
		clearSnapshots
		clearStopWatch
	}

}

trait TransactionContext extends PropagationContext {

	protected[fwbrasil] val transactionManager =
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
			throw new IllegalStateException("There is another active transaction!")
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