package net.fwbrasil.radon

import net.fwbrasil.radon.TestRadonContext._

object PerformanceTest extends App {

	val num = 100000

	def create =
		transactional {
			for (i <- 0 until num)
				yield new Ref(i)
		}

	def modify(refs: Seq[Ref[Int]]) =
		transactional {
			refs.foreach(ref => ref := !ref + 1)
		}

	val refs = create
	modify(refs)

}

object SimpleMain extends App {
	val ref = transactional(new Ref(0))
	transactional(ref := !ref + 1)
}