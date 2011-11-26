package net.fwbrasil.radon.util

import scala.collection._

trait ExclusiveThreadLocalItem {
  private[util] var boundThread: Option[Thread] = None
  private[util] def setBoundThread(thread: Thread) =
    this.synchronized {
      boundThread = Option(thread)
    }
}

class ExclusiveThreadLocal[T <: ExclusiveThreadLocalItem] {

  val underlying = new ThreadLocal[Option[T]]

  def get: Option[T] = {
	  val underlyingGet = underlying.get
	  if(underlyingGet==null)
	 	  None
 	  else 
 	 	  underlyingGet
  }

  def set(value: Option[T]) = {
    val currentThread = Thread.currentThread
    if (value == null || value == None) {
      underlying.set(None)
    } else {
      val actualBoundThread = value.get.boundThread
      if (actualBoundThread != None && currentThread != actualBoundThread.get)
        throw new IllegalStateException(
          "ExclusiveThreadLocal: value is bound to another thread.")
      underlying.set(value)
      value.get.setBoundThread(currentThread)
    }
  }

}