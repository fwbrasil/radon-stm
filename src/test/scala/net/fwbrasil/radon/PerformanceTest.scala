package net.fwbrasil.radon

import net.fwbrasil.radon.TestRadonContext._

object PerformanceTest extends App {

	val num = 10000

	def create =
		transactional {
			for (i <- 0 to num)
				yield new Ref(i)
		}

	create

}