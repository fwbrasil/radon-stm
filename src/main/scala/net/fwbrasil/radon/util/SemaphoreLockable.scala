package net.fwbrasil.radon.util

import scala.collection._
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import scala.collection.mutable.{ Set => MutableSet}
import java.util.concurrent.atomic.AtomicReferenceArray
import System.{ identityHashCode => identity }
import java.util.concurrent.Semaphore

private[fwbrasil] trait SemaphoreLockable {

    private var semaphore = new Semaphore(Int.MaxValue, true)

    private[fwbrasil] def tryReadLock =
        semaphore.tryAcquire(1)

    private[fwbrasil] def tryWriteLock=
        semaphore.tryAcquire(Int.MaxValue)
    
    private[fwbrasil] def readUnlock=
        semaphore.release

    private[fwbrasil] def writeUnlock=
        semaphore.release(Int.MaxValue)

}

object SemaphoreLockable {
    def lockall[L <% SemaphoreLockable](lockables: Iterable[L], lockFunc: (SemaphoreLockable) => Boolean) =
        lockables.toList.partition(lockFunc(_))
}