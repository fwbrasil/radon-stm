package net.fwbrasil.radon.util

import org.specs2.mutable._

trait Shield {
	def cleanTest[A](timeout: Long)(f: => A) = 
		f
}

trait TestShield extends TestThreadShield {
	this: Specification =>
}

trait TestThreadShield extends Shield {
	this: Specification =>
	
	override def cleanTest[A](timeout: Long)(f: => A) = {
		val threadsBefore = currentThreads
		val result = super.cleanTest(timeout)(f)
		val threadsAfter = currentThreads.toList 
		threadsAfter must haveTheSameElementsAs(threadsBefore.toList)
		result
	}
	
	private[this] def currentThreads = 
		Set(Thread.getAllStackTraces().keySet()).toList
}

trait TestConsoleShield extends Shield {
	this: Specification =>
}