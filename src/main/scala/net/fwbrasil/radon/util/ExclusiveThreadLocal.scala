package net.fwbrasil.radon.util

import scala.collection._

trait ExclusiveThreadLocalItem {
	private[util] var boundThread: Option[Thread] = None
	private[util] def setBoundThread(thread: Option[Thread]) =
		this.synchronized {
			boundThread = thread
		}
}

class ExclusiveThreadLocal[T <: ExclusiveThreadLocalItem] {

	val underlying = new ThreadLocal[Option[T]]

	def get: Option[T] = {
		val underlyingGet = underlying.get
		if (underlyingGet == null)
			None
		else
			underlyingGet
	}

	def set(value: Option[T]) = {
		require(value != null && value.isDefined)
		val currentThread = Thread.currentThread
		val actualBoundThread = value.get.boundThread
		if (actualBoundThread != None && currentThread != actualBoundThread.get)
			throw new IllegalStateException(
				"ExclusiveThreadLocal: value is bound to another thread.")
		underlying.set(value)
		value.get.setBoundThread(Some(currentThread))
	}

	def clean(value: Option[T]) = {
		underlying.set(None)
		if (value.isDefined)
			value.get.setBoundThread(None)
	}

}