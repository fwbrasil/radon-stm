package net.fwbrasil.radon.problems

import net.fwbrasil.radon.TestRadonContext._
import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import net.fwbrasil.radon.dsl.actor._
import scala.util.Random
import scala.collection.mutable.Stack
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner]) /**
 * See http://en.wikipedia.org/wiki/Dining_philosophers_problem
 *  The STM solution is particularly straightforward because we can
 *  simultaneously pick up two forks.
 */
class DiningPhilosophers extends Specification {

    "DiningPhilosophers" should {
        "work" in {
            val meals = 1000
            for (p <- 0 until 1) {
                val elapsed = time(5, meals)
                printf("%3.1f usec/meal\n", (elapsed * 1000.0) / meals)
            }
            true must beTrue
        }
    }

    class Fork {
        val inUse =
            transactional {
                new Ref(false)
            }
    }

    class PhilosopherThread(meals: Int, left: Fork, right: Fork) extends Thread {
        override def run() {
            for (m <- 0 until meals) {
                transactional {
                    // THINK
                    if (left.inUse || right.inUse)
                        retry(left.inUse, right.inUse)
                    left.inUse := true
                    right.inUse := true
                }
                transactional {
                    // EAT
                    left.inUse := false
                    right.inUse := false
                }
            }
        }
    }

    def time(tableSize: Int, meals: Int): Long = {
        val forks = Array.tabulate(tableSize) { _ => new Fork }
        val threads = Array.tabulate(tableSize) { i => new PhilosopherThread(meals, forks(i), forks((i + 1) % tableSize)) }
        val start = System.currentTimeMillis
        for (t <- threads) t.start()
        for (t <- threads) t.join()
        System.currentTimeMillis - start
    }

}
