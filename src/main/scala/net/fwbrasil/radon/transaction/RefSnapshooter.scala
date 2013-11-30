package net.fwbrasil.radon.transaction

import java.util.IdentityHashMap
import net.fwbrasil.radon.transaction.time.TransactionStopWatch
import net.fwbrasil.radon.ref.Ref

class RefSnapshot(val ref: Ref[Any]) {
    val originalContent = ref.refContent
    var value = originalContent.value
    var destroyedFlag = originalContent.destroyedFlag
    var isReadDestroyedFlag = false
    var isRead = false
    var isWrite = false
}

abstract class RefSnapshooter extends TransactionStopWatch {

    val transactionId = Transaction.nextId

    private[fwbrasil] var refsSnapshot = new IdentityHashMap[Ref[Any], RefSnapshot]()

    protected def getSnapshot(ref: Ref[Any]): RefSnapshot = {
        startIfNotStarted
        val snapOrNull = refsSnapshot.get(ref)
        if (snapOrNull == null) {
            val newSnap = new RefSnapshot(ref)
            refsSnapshot.put(ref, newSnap)
            newSnap
        } else 
            snapOrNull
    }

    protected def snapshotRead(ref: Ref[Any]): Option[Any] = {
        val snap = getSnapshot(ref)
        snap.isRead = true
        snap.value
    }
    
    protected def snapshotReadOriginalValue(ref: Ref[Any]): Option[Any] = {
        val snap = getSnapshot(ref)
        snap.isRead = true
        snap.originalContent.value
    }
    
    protected def snapshotDestroy(ref: Ref[Any]): Unit = {
        val snap = getSnapshot(ref)
        snap.destroyedFlag = true
        snap.isWrite = true
    }
    
    protected def snapshotIsDestroyed(ref: Ref[Any]): Boolean = {
        val snap = getSnapshot(ref)
        snap.isReadDestroyedFlag = true
        snap.destroyedFlag
    }

    protected def snapshotWrite(ref: Ref[Any], value: Option[Any]): Unit = {
        val snap = getSnapshot(ref)
        snap.value = value
        snap.isWrite = true
    }

    private[transaction] def clearSnapshots =
        refsSnapshot.clear
}
