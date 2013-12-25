package net.fwbrasil.radon.util

import scala.collection._
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import scala.collection.mutable.{ Set => MutableSet }
import java.util.concurrent.atomic.AtomicReferenceArray
import System.{ identityHashCode => identity }
import java.util.concurrent.Semaphore

private[fwbrasil] trait Lockable {

    private val semaphore = new Semaphore(Int.MaxValue, true)

    private[fwbrasil] def tryReadLock =
        semaphore.tryAcquire(1)

    private[fwbrasil] def tryWriteLock =
        semaphore.tryAcquire(Int.MaxValue)

    private[fwbrasil] def readLock =
        semaphore.acquire(1)

    private[fwbrasil] def writeLock =
        semaphore.acquire(Int.MaxValue)

    private[fwbrasil] def readUnlock =
        semaphore.release

    private[fwbrasil] def writeUnlock =
        semaphore.release(Int.MaxValue)

    private[fwbrasil] def readLockCount =
        if (semaphore.availablePermits > 0)
            Int.MaxValue - semaphore.availablePermits
        else 0

    private[fwbrasil] def isWriteLocked =
        semaphore.availablePermits == 0

    private[fwbrasil] def doWithReadLock[A](f: => A): A =
        try {
            readLock
            f
        } finally
            readUnlock

    private[fwbrasil] def doWithWriteLock[A](f: => A): A =
        try {
            writeLock
            f
        } finally
            writeUnlock
}

object Lockable {
    def lockall[L <% Lockable](pLockables: Iterable[L], lockFunc: (Lockable) => Boolean) = {
        val lockables = pLockables.toList
        val unlocked = lockables.filter(!lockFunc(_)).toList
        if(unlocked.isEmpty)
            (lockables, List())
        else
            (lockables.filter(!unlocked.contains(_)), unlocked.toList)
    }
}