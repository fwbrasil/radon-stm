package net.fwbrasil.radon.util

import scala.collection._
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer

private[fwbrasil] trait Lockable {

	private[this] val reentrantReadWriteLock = new ReentrantReadWriteLock(false)
	private[this] val reentrantReadLock = reentrantReadWriteLock.readLock
	private[this] val reentrantWriteLock = reentrantReadWriteLock.writeLock

	private[fwbrasil] def tryReadLock =
		reentrantReadLock.tryLock

	private[fwbrasil] def tryWriteLock =
		reentrantWriteLock.tryLock

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
	def lockall[L <% Lockable](lockables: Iterable[L], lockFunc: (Lockable) => Boolean) = {
		val locked = ListBuffer[L]()
		val unlocked = ListBuffer[L]()
		for (lockable <- lockables)
			if (lockFunc(lockable))
				locked += lockable
			else
				unlocked += lockable
		(locked.toList, unlocked.toList)
	}
}