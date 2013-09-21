
package net.fwbrasil.radon.transaction

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success
import net.fwbrasil.radon.ref.Ref
import net.fwbrasil.radon.ref.RefContent
import net.fwbrasil.radon.util.ExclusiveThreadLocalItem
import net.fwbrasil.radon.util.Lockable.lockall
import java.util.concurrent.atomic.AtomicLong

class Transaction(val transient: Boolean)(implicit val context: TransactionContext)
        extends TransactionValidator
        with ExclusiveThreadLocalItem {

    def this()(implicit context: TransactionContext) = this(false)

    import context._

    private var refsRead: ListBuffer[Ref[Any]] = _
    private var refsReadOnly: ListBuffer[Ref[Any]] = _
    private var refsWrite: ListBuffer[Ref[Any]] = _
    private var snapshots: List[RefSnapshot] = _
    private var readLocks: List[Ref[Any]] = _
    private var writeLocks: List[Ref[Any]] = _
    var attachments = new ListBuffer[Any]()

    def reads =
        refsRead

    def assignments =
        if (snapshots != null)
            for (snapshot <- snapshots if (snapshot.isWrite == true)) yield (snapshot.ref, snapshot.value, snapshot.destroyedFlag)
        else
            List()

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

    private[radon] def isDestroyed[T](ref: Ref[T]): Boolean = 
        snapshotIsDestroyed(ref.asInstanceOf[Ref[Any]])

    private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
        val snap = refsSnapshot.get(ref)
        snap != null && snap.isWrite
    }

    def commit(): Unit =
        commit(rollback = false)

    def asyncCommit()(implicit ectx: ExecutionContext): Future[Unit] =
        asyncCommit(rollback = false)

    private def updateReadsAndWrites = {
        import scala.collection.JavaConversions._
        val refsRead = new ListBuffer[Ref[Any]]()
        val refsReadOnly = new ListBuffer[Ref[Any]]()
        val refsWrite = new ListBuffer[Ref[Any]]()
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
        this.refsRead = refsRead
        this.refsReadOnly = refsReadOnly
        this.refsWrite = refsWrite
        this.snapshots = snapshots
    }

    private def acquireLocks = {
        val (readLockeds, readUnlockeds) = lockall(refsReadOnly, _.tryReadLock)
        val (writeLockeds, writeUnlockeds) = lockall(refsWrite, _.tryWriteLock)
        readLocks = readLockeds
        writeLocks = writeLockeds
        retryIfTrue(readUnlockeds.nonEmpty, readUnlockeds)
        retryIfTrue(writeUnlockeds.nonEmpty, writeUnlockeds)
    }

    private def freeLocks = {
        if (writeLocks != null)
            writeLocks.foreach(_.writeUnlock)
        if (readLocks != null)
            readLocks.foreach(_.readUnlock)
        writeLocks = null
        readLocks = null
    }

    private def commit(rollback: Boolean): Unit = {
        updateReadsAndWrites
        startIfNotStarted
        try {
            acquireLocks
            validateTransaction
            if (!transient && !rollback)
                context.makeDurable(this)
        } catch {
            case e: Throwable =>
                prepareRollback
                throw e
        } finally
            flushTransaction
    }

    private def asyncCommit(rollback: Boolean)(implicit ectx: ExecutionContext): Future[Unit] = {
        updateReadsAndWrites
        startIfNotStarted
        Future {
            acquireLocks
            validateTransaction
        }.flatMap { _ =>
            if (!transient && !rollback)
                context.makeDurableAsync(this)
            else
                Future.successful()
        }.transform(
            {
                _ => flushTransaction
            }, {
                e =>
                    prepareRollback
                    flushTransaction
                    throw e
            })
    }

    private def flushTransaction = {
        startIfNotStarted
        stop
        flushToMemory
        freeLocks
        clear
    }

    private def flushToMemory = {
        val snapshotsIterator = snapshots.iterator
        snapshots = List()
        snapshotsIterator.foreach(setRefContent)
    }

    private def validateTransaction = {
        refsReadOnly.foreach(e => {
            validateContext(e)
            validateConcurrentRefCreation(e)
        })

        refsRead.foreach { e =>
            validateRead(e)
            validateDestroyed(e)
        }

        refsWrite.foreach(e => {
            validateContext(e)
            validateConcurrentRefCreation(e)
            validateWrite(e)
            validateDestroyed(e)
        })
    }

    private def setRefContent(snapshot: RefSnapshot) = {
        val ref = snapshot.ref
        val refContent = ref.refContent
        var value: Option[Any] = None
        var destroyedFlag = false
        if (snapshot.isWrite && refContent.writeTimestamp < startTimestamp) {
            value = snapshot.value
            destroyedFlag = snapshot.destroyedFlag
        } else {
            value = refContent.value
            destroyedFlag = refContent.destroyedFlag
        }
        val read =
            readTimestamp(snapshot.isRead, refContent)
        val write =
            writeTimestamp(snapshot.isWrite, refContent)
        require(((ref.creationTransactionId != transactionId || write != 0) &&
            write != Long.MaxValue) || transient)
        ref.setRefContent(value, read, write, destroyedFlag)
    }

    private def readTimestamp(isRefRead: Boolean, refContent: RefContent[_]) =
        if (isRefRead && refContent.readTimestamp < startTimestamp)
            startTimestamp
        else
            refContent.readTimestamp

    private def writeTimestamp(isRefWrite: Boolean, refContent: RefContent[_]) =
        if (isRefWrite && refContent.writeTimestamp < startTimestamp)
            endTimestamp
        else
            refContent.writeTimestamp

    def prepareRollback = {
        val refsWrote =
            if (refsWrite != null)
                refsWrite
            else
                new ListBuffer[Ref[Any]]()
        val refsCreated =
            refsWrote.filter(_.creationTransactionId == transactionId)
        clear
        for (ref <- refsCreated)
            destroy(ref)
        updateReadsAndWrites
    }

    def rollback() = {
        updateReadsAndWrites
        prepareRollback
        commit(rollback = true)
    }

    private[transaction] def clear = {
        refsRead = null
        refsWrite = null
        attachments.clear
        clearSnapshots
        clearStopWatch
    }

}

object Transaction {
    private val lastId = new AtomicLong(0)
    def nextId = lastId.incrementAndGet
}