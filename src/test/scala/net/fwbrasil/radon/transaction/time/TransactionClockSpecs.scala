package net.fwbrasil.radon.transaction.time

import org.specs2.mutable._
import net.fwbrasil.radon.util.ThreadUtil._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class TransactionClockSpecs extends Specification {

    "A transactional clock" should {
        "have unique ticks" in {
            val clock = new TransactionClock
            val start = clock.tick
            val threads = 50
            val loops = 500
            var running = List[Thread]()
            for (_ <- 0 until threads) {
                val thread = runInNewThread {
                    for (_ <- 0 until loops) {
                        clock.tick
                    }
                }
                running ::= thread
            }
            running.foreach(_.join)
            val end = clock.tick
            (end - start - 1) mustEqual threads * loops
        }
    }
}