package net.fwbrasil.radon.util

import java.util.concurrent.atomic.AtomicLong

class Statistic(val name: String) {
	val count =
		new AtomicLong(0)
	def increment = {
		Statistics.touch
		count.incrementAndGet
	}
	def decrement = {
		Statistics.touch
		count.decrementAndGet
	}
	def print =
		println(name + ": " + count.get)
}

object Statistics {

	val retryCount =
		new Statistic("retryCount")

	val retryWithWriteCount =
		new Statistic("retryWithWriteCount")

	val commitCount =
		new Statistic("commitCount")

	val waitingCount =
		new Statistic("waitingCount")

	var lastPrint = System.currentTimeMillis

	def touch =
		if (false && (System.currentTimeMillis - lastPrint) > 1000) {
			println("**********STATISTICS**********")
			List(retryCount, retryWithWriteCount, commitCount, waitingCount).foreach(_.print)
			println("********END STATISTICS********")
			lastPrint = System.currentTimeMillis
		}

}