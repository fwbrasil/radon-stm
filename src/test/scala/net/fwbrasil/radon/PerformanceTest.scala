package net.fwbrasil.radon

import net.fwbrasil.radon.TestRadonContext._

object PerformanceTest extends App {

	val num = 100000

	def create =
		transactional {
			for (i <- 0 until num)
				yield new Ref(i)
		}

	create

}