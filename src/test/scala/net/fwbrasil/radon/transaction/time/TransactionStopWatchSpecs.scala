package net.fwbrasil.radon.transaction.time

import org.specs2.mutable._
import net.fwbrasil.radon.transaction.TransactionContext
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class TransactionStopWatchSpecs extends Specification {

    "The Transaction stop watch" should {

        "have start timestamp 0 when created" in {
            val transactionStopWatch = newTransactionStopWatch
            transactionStopWatch.startTimestamp must beEqualTo(0)
        }

        "have start timestamp Long.MaxValue when created" in {
            val transactionStopWatch = newTransactionStopWatch
            transactionStopWatch.endTimestamp must beEqualTo(Long.MaxValue)
        }

        "start with the current clock tick" in {
            val transactionStopWatch = newTransactionStopWatch
            val tick = transactionStopWatch.context.transactionClock.tick
            transactionStopWatch.start
            transactionStopWatch.startTimestamp must beEqualTo(tick + 1)
            transactionStopWatch.endTimestamp must beEqualTo(Long.MaxValue)
        }

        "stop with the current clock tick" in {
            val transactionStopWatch = newTransactionStopWatch
            val tick = transactionStopWatch.context.transactionClock.tick
            transactionStopWatch.start
            transactionStopWatch.stop
            transactionStopWatch.startTimestamp must beEqualTo(tick + 1)
            transactionStopWatch.endTimestamp must beEqualTo(tick + 2)
        }

        "do not stop without start" in {
            val transactionStopWatch = newTransactionStopWatch
            val tick = transactionStopWatch.context.transactionClock.tick
            transactionStopWatch.stop must throwA[IllegalStateException]
        }

        "do not start if alredy started" in {
            val transactionStopWatch = newTransactionStopWatch
            val tick = transactionStopWatch.context.transactionClock.tick
            transactionStopWatch.start
            transactionStopWatch.start must throwA[IllegalStateException]
        }

        "do not stop if alredy stoped" in {
            val transactionStopWatch = newTransactionStopWatch
            val tick = transactionStopWatch.context.transactionClock.tick
            transactionStopWatch.start
            transactionStopWatch.stop
            transactionStopWatch.stop must throwA[IllegalStateException]
        }
    }

    def newTransactionStopWatch =
        new TransactionStopWatch {
            val context = new TransactionContext {}
        }
}