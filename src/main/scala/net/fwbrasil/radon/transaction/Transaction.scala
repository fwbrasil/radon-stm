
package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.ConcurrentTransactionException
import net.fwbrasil.radon.RadonContext
import net.fwbrasil.radon.ref.Ref
import net.fwbrasil.radon.ref.RefContent
import net.fwbrasil.radon.util.ExclusiveThreadLocalItem
import net.fwbrasil.radon.util.Debug
import net.fwbrasil.radon.util.Lockable._
import net.fwbrasil.radon.transaction.time._
import java.util.IdentityHashMap
import java.util.HashMap
import java.util.{ HashSet => JHashSet }
import scala.collection.mutable.ListBuffer

class RefSnapshot(val ref: Ref[Any]) {
	val originalContent = ref.refContent
	var value = originalContent.value
	var destroyedFlag = originalContent.destroyedFlag
	var isRead = false
	var isWrite = false
}

trait RefSnapshooter {
	this: Transaction =>

	private[transaction] var refsSnapshot = new IdentityHashMap[Ref[Any], RefSnapshot]()

	private[transaction] def isAValidSnapshot(snapshot: RefSnapshot) =
		snapshot.originalContent.writeTimestamp < startTimestamp

	private[transaction] def isAnOutdatedSnapshot(ref: Ref[Any], snapshot: RefSnapshot) = {
		val refContent = snapshot.originalContent
		(refContent.writeTimestamp != ref.refContent.writeTimestamp
			&& refContent.readTimestamp != ref.refContent.readTimestamp)
	}

	protected def getSnapshot(ref: Ref[Any]): RefSnapshot =
		getSnapshot(ref, true)

	protected def getSnapshot(ref: Ref[Any], validateDestroyed: Boolean): RefSnapshot = {
		if (validateDestroyed)
			startIfNotStarted
		val snapOrNull = refsSnapshot.get(ref)
		val snap =
			if (snapOrNull == null) {
				val newSnap = new RefSnapshot(ref)
				refsSnapshot.put(ref, newSnap)
				newSnap
			} else
				snapOrNull
		if (validateDestroyed)
			validateIfDestroyed(snap)
		snap
	}

	private[transaction] def snapshotRead(ref: Ref[Any]): Option[Any] = {
		val snap = getSnapshot(ref)
		snap.isRead = true
		snap.value
	}

	private[transaction] def snapshotDestroy(ref: Ref[Any]): Unit = {
		val snap = getSnapshot(ref)
		snap.destroyedFlag = true
		snap.isWrite = true
	}

	private[transaction] def snapshotWrite(ref: Ref[Any], value: Option[Any]): Unit = {
		val snap = getSnapshot(ref)
		snap.value = value
		snap.isWrite = true
	}

	private def validateIfDestroyed(snapshot: RefSnapshot) =
		if (snapshot.destroyedFlag)
			throw new IllegalStateException("Triyng to access a destroyed ref.")

	private[transaction] def clearSnapshots =
		refsSnapshot = new IdentityHashMap[Ref[Any], RefSnapshot]()
}

trait TransactionValidator {
	this: Transaction =>

	private[transaction] def validateWrite(ref: Ref[Any]) =
		retryIfTrue(isRefReadAfterTheStartOfTransaction(ref) || isRefDestroyedAfterTheStartOfTransaction(ref), ref)

	private[transaction] def validateRead(ref: Ref[Any]) = {
		val snapshot = getSnapshot(ref, false)
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

	private var refsRead = List[Ref[Any]]()
	private var refsReadOnly = List[Ref[Any]]()
	private var refsWrite = List[Ref[Any]]()
	private var snapshots = List[RefSnapshot]()

	def reads =
		refsRead

	def assignments =
		for (snapshot <- snapshots if (snapshot.isWrite == true)) yield (snapshot.ref, snapshot.value, snapshot.destroyedFlag)

	private[transaction] def isReadOnly =
		!isRetryWithWrite && refsWrite.isEmpty

	private[transaction] def retryIfTrue(condition: Boolean, refs: Ref[_]*) =
		if (condition)
			retry(refs: _*)

	private[radon] def put[T](ref: Ref[T], value: Option[T]) = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		snapshotWrite(anyRef, value)
	}

	private[radon] def get[T](ref: Ref[T]): Option[T] =
		snapshotRead(ref.asInstanceOf[Ref[Any]]).asInstanceOf[Option[T]]

	private[radon] def destroy[T](ref: Ref[T]): Unit = {
		val anyRef = ref.asInstanceOf[Ref[Any]]
		snapshotDestroy(anyRef)
	}

	private[radon] def isDestroyed[T](ref: Ref[T]): Boolean = {
		startIfNotStarted
		val snap = getSnapshot(ref.asInstanceOf[Ref[Any]], false)
		snap.isRead = true
		snap.destroyedFlag
	}

	private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
		val snap = getSnapshot(ref.asInstanceOf[Ref[Any]])
		snap.isRead = true
		snap.isWrite
	}

	def commit(): Unit =
		commit(rollback = false)

	private def updateReadsAndWrites = {
		import scala.collection.JavaConversions._
		val refsRead = ListBuffer[Ref[Any]]()
		val refsReadOnly = ListBuffer[Ref[Any]]()
		val refsWrite = ListBuffer[Ref[Any]]()
		val snapshots = refsSnapshot.values.toList
		for (snapshot <- snapshots) {
			val ref = snapshot.ref
			if (snapshot.isRead) {
				refsRead += ref
				if (!snapshot.isWrite)
					refsReadOnly += ref
			}
			if (snapshot.isWrite)
				refsWrite += ref
		}
		this.refsRead = refsRead.toList
		this.refsReadOnly = refsReadOnly.toList
		this.refsWrite = refsWrite.toList
		this.snapshots = snapshots
	}

	private def commit(rollback: Boolean): Unit = {
		updateReadsAndWrites
		try {
			val (readLockeds, readUnlockeds) = lockall(refsReadOnly, _.tryReadLock)
			try {
				val (writeLockeds, writeUnlockeds) = lockall(refsWrite, _.tryWriteLock)
				try {

					startIfNotStarted
					stop

					try {
						retryIfTrue(readUnlockeds.nonEmpty, readUnlockeds.toSeq: _*)
						retryIfTrue(writeUnlockeds.nonEmpty, writeUnlockeds.toSeq: _*)

						refsReadOnly.foreach(e => {
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
					} finally
						snapshots.foreach(setRefContent)
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

	private[this] def setRefContent(snapshot: RefSnapshot) =
		if (!transient) {
			val ref = snapshot.ref
			val refContent = ref.refContent
			val (value, destroyedFlag) =
				valueAndDestroyedFlag(snapshot, refContent)
			val read =
				readTimestamp(snapshot.isRead, refContent)
			val write =
				writeTimestamp(snapshot.isWrite, refContent)
			require(ref.creationTransaction != this || write != 0)
			ref.setRefContent(value, read, write, destroyedFlag)
		}

	private[this] def valueAndDestroyedFlag(snapshot: RefSnapshot, refContent: RefContent[_]) =
		if (snapshot.isWrite && refContent.writeTimestamp < startTimestamp)
			(snapshot.value, snapshot.destroyedFlag)
		else
			(refContent.value, refContent.destroyedFlag)

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
		val refsWrote = refsWrite
		val refsCreated =
			refsWrote.filter(_.creationTransaction == this)
		clear
		for (ref <- refsCreated)
			destroy(ref)
		for (ref <- refsWrote)
			ref.notifyRollback
		updateReadsAndWrites
	}

	def rollback() = {
		updateReadsAndWrites
		prepareRollback
		commit(rollback = true)
	}

	private[transaction] def clear = {
		refsRead = List()
		refsWrite = List()
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