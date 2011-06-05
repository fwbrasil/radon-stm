package net.fwbrasil.radon.transaction.time

import java.util.concurrent.atomic.AtomicLong

class TransactionClock {

	private[this] val current = new AtomicLong(0)
	private[radon] def tick = current.incrementAndGet

}