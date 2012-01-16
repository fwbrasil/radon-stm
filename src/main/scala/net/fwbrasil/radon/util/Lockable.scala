package net.fwbrasil.radon.util

import scala.collection._
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit

private[fwbrasil] trait Lockable {

	private[this] val reentrantReadWriteLock = new ReentrantReadWriteLock(false)
	private[this] val reentrantReadLock = reentrantReadWriteLock.readLock
	private[this] val reentrantWriteLock = reentrantReadWriteLock.writeLock

	private[fwbrasil] def tryReadLock =
		reentrantReadLock.tryLock //(1, TimeUnit.MICROSECONDS)

	private[fwbrasil] def tryWriteLock =
		reentrantWriteLock.tryLock //(1, TimeUnit.MICROSECONDS)

	private[fwbrasil] def readLock = {
		reentrantReadLock.lock
		true
	}

	private[fwbrasil] def writeLock = {
		reentrantWriteLock.lock
		true
	}

	private[fwbrasil] def readUnlock = {
		reentrantReadLock.unlock
		true
	}

	private[fwbrasil] def writeUnlock = {
		reentrantWriteLock.unlock
		true
	}

	private[fwbrasil] def readLockCount =
		reentrantReadWriteLock.getReadLockCount

	private[fwbrasil] def isWriteLocked =
		reentrantReadWriteLock.isWriteLocked

	private[fwbrasil] def doWithReadLock[A](f: => A): A =
		try {
			readLock
			f
		} finally {
			readUnlock
		}

	private[fwbrasil] def doWithWriteLock[A](f: => A): A =
		try {
			writeLock
			f
		} finally {
			writeUnlock
		}
}

object Lockable {
	def lockall[L <% Lockable](lockables: Set[L], lockFunc: (Lockable) => Boolean) =
		for (lockable <- lockables if (!lockFunc(lockable)))
			yield lockable
}