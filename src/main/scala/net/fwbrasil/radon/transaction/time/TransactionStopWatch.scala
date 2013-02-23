package net.fwbrasil.radon.transaction.time

import net.fwbrasil.radon.transaction.TransactionContext

abstract class TransactionStopWatch {

    private[transaction] implicit val context: TransactionContext

    private[fwbrasil] var startTimestamp = 0l
    private[fwbrasil] var endTimestamp = Long.MaxValue

    def startIfNotStarted() =
        if (startTimestamp == 0l)
            start

    private[transaction] def start =
        if (started)
            throw new IllegalStateException("TransactionStopWatch alredy started.")
        else
            startTimestamp = context.transactionClock.tick

    private[transaction] def stop =
        if (stoped)
            throw new IllegalStateException("TransactionStopWatch alredy stoped.")
        else if (!started)
            throw new IllegalStateException("TransactionStopWatch isn't started.")
        else
            endTimestamp = context.transactionClock.tick

    private[transaction] def started =
        startTimestamp != 0

    private[transaction] def stoped =
        endTimestamp != Long.MaxValue

    private[transaction] def clearStopWatch = {
        startTimestamp = 0l
        endTimestamp = Long.MaxValue
    }

}