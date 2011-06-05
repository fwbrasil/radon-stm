package net.fwbrasil.radon.problems

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import net.fwbrasil.radon.dsl.actor._
import scala.util.Random
import scala.collection.immutable.Stack
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class ProducerConsumer extends Specification {

	class Message

	case class Buffer(size: Int) extends Ref[Stack[Message]](Stack[Message]()) {
		def push(message: Message) = {
			if (isFull)
				throw new IllegalStateException("Buffer is full!")
			super.get.get.push(message)
		}
		def pop = {
			if (isEmpty)
				throw new IllegalStateException("Buffer is empty!")
			super.get.get.pop
		}

		def isEmpty =
			super.get.get.isEmpty

		def isFull =
			super.get.get.size == size
	}

	case class Producer(messagesToSend: Stack[Message], buffer: Buffer) extends Thread {
		override def run = {
			transactionalWhile(!messagesToSend.isEmpty) {
				if (!buffer.isFull)
					buffer.push(messagesToSend.pop)
				else
					retry(buffer)
			}
		}
	}

	case class Consumer(fStop: () => Boolean, messagesReceived: Stack[Message], buffer: Buffer) extends Thread {
		override def run = {
			transactionalWhile(!fStop() || buffer.nonEmpty) {
				if (buffer.nonEmpty)
					messagesReceived.push(buffer.pop)
				else
					retry(buffer)
			}
		}
	}

	"ProducerConsumer" in {
		val messagesStack = Stack[Message]()
		for (i <- 0 to 100)
			messagesStack.push(new Message)
		val buffer = transactional {
			new Buffer(10)
		}
		val producer = Producer(messagesStack, buffer)
		var stop = false
		val consumer = Consumer(() => stop, Stack(), buffer)
		producer.start
		consumer.start
		producer.join
		transactionalWhile(buffer.nonEmpty) {}
		val a= transactional {
			buffer.nonEmpty
		}
		stop = true
		consumer.join
		transactional {
			producer.messagesToSend.isEmpty
		} must beTrue
		transactional {
			consumer.messagesReceived.toSet
		} must beEqualTo(messagesStack.toSet)
	}
}