
package net.fwbrasil.radon.transaction

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.mutable.ListBuffer

import net.fwbrasil.radon.ref.Ref
import net.fwbrasil.radon.ref.RefContent
import net.fwbrasil.radon.util.ExclusiveThreadLocalItem
import net.fwbrasil.radon.util.Lockable.lockall

class Transaction(val transient: Boolean)(implicit val context: TransactionContext)
        extends TransactionValidator
        with ExclusiveThreadLocalItem {

    def this()(implicit context: TransactionContext) = this(false)

    import context._

    private[radon] var isRetryWithWrite = false

    private var refsRead: ListBuffer[Ref[Any]] = _
    private var refsReadOnly: ListBuffer[Ref[Any]] = _
    private var refsWrite: ListBuffer[Ref[Any]] = _
    private var snapshots: List[RefSnapshot] = _

    def reads =
        refsRead

    def assignments =
        if (snapshots != null)
            for (snapshot <- snapshots if (snapshot.isWrite == true)) yield (snapshot.ref, snapshot.value, snapshot.destroyedFlag)
        else
            List()

    protected def isReadOnly =
        !isRetryWithWrite && refsWrite.isEmpty

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
        lazy val snap = refsSnapshot.get(ref)
        ref.refContent.destroyedFlag || (snap != null && snap.destroyedFlag)
    }

    private[radon] def isDirty[T](ref: Ref[T]): Boolean = {
        val snap = refsSnapshot.get(ref)
        snap != null && snap.isWrite
    }

    def commit(): Unit =
        commit(rollback = false)

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

    private def commit(rollback: Boolean): Unit = {
        updateReadsAndWrites
        try {
            val (readLockeds, readUnlockeds) = lockall(refsReadOnly, _.tryReadLock)
            try {
                val (writeLockeds, writeUnlockeds) = lockall(refsWrite, _.tryWriteLock)
                try {

                    startIfNotStarted

                    try {
                        retryIfTrue(readUnlockeds.nonEmpty, readUnlockeds)
                        retryIfTrue(writeUnlockeds.nonEmpty, writeUnlockeds)

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
                        case e: Throwable =>
                            prepareRollback
                            throw e
                    } finally {
                        startIfNotStarted
                        stop
                        val snapshotsIterator = snapshots.iterator
                        snapshots = List()
                        snapshotsIterator.foreach(setRefContent)
                    }
                } finally
                    writeLockeds.foreach(_.writeUnlock)
            } finally
                readLockeds.foreach(_.readUnlock)
        } finally
            clear
    }

    private[this] def setRefContent(snapshot: RefSnapshot) = {
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
        require(((ref.creationTransaction != this || write != 0) &&
            write != Long.MaxValue) || transient)
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
        val refsWrote =
            if (refsWrite != null)
                refsWrite
            else
                new ListBuffer[Ref[Any]]()
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
        refsRead = null
        refsWrite = null
        clearSnapshots
        clearStopWatch
    }

}
