package net.fwbrasil.radon

import net.fwbrasil.radon.TestRadonContext._
import org.specs2.mutable._

import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class STMSpecs extends Specification {

	"refs" should {
		"retrieve values" in transactional {
			val ref = new Ref(42) {}

			ref.get.get mustEqual 42
			!ref mustEqual 42
			(refToValue(ref): Int) mustEqual 42
		}

		"mutate values" in transactional {
			val ref = new Ref(42)
			!ref mustEqual 42
			ref := 6
			!ref mustEqual 6
		}
	}

	"transactionals" should {
		"return values" in transactional {
			val ref = new Ref(42)
			!ref mustEqual 42
		}

		"isolate changes" in {
			val ref = transactional {
				new Ref(42)
			}

				def check = transactional(requiresNew) {
					!ref mustEqual 42
				}

				def modify = transactional {
					!ref mustEqual 42

					ref := 12

					!ref mustEqual 12
				}
			modify

			transactional {
				!ref mustEqual 12
			}
		}
		"detect conflicts" in {
			val ref = transactional {
				new Ref(42)
			}

				def modify {
					ref := 10 + ref
				}

			val right = thread {
				transactional(modify)
			}

			var runs = 0
				def modify2 {
					runs += 1
					if (runs == 1) {
						right.start()
						right.join()
					}
					ref := 10 + ref
				}

			val left = thread {
				transactional(modify2)
			}

			left.start()
			left.join()

			transactional {
				!ref mustEqual 62
			}
			runs mustEqual 1 //Consigo evitar este conflito
		}
	}

	"maintain separate versions" in {
		val ref = transactional {
			new Ref(42)
		}

			def modify1 {
				ref := 21
			}

		val left = thread {
			transactional(modify1)
		}

		var runs = 0
			def modify2 {
				runs += 1

				if (runs == 1) {
					!ref mustEqual 42
					left.start
					left.join
				} else {
					!ref mustEqual 21
				}

				ref := 10 + ref
			}

		val right = thread {
			transactional(modify2)
		}

		right.start()

		right.join()

		transactional {
			!ref mustEqual 31
		}
		runs mustEqual 2
	}

	"throw exception if transaction is used in multiple threads" in {
		val t = new Transaction
		var error = false
		val ref = transactional(t) {
			new Ref(42)
		}

			def modify1 {
				ref := 21
			}

		val left = thread {
			try {
				transactional(t)(modify1)
			} catch {
				case e: IllegalStateException => error = true
			}
		}

		var runs = 0
			def modify2 {
				runs += 1

				if (runs == 1) {
					!ref mustEqual 42
					left.start
					left.join
				} else {
					!ref mustEqual 21
				}

				ref := 10 + ref
			}

		val right = thread {
			try {
				transactional(t)(modify2)
			} catch {
				case e: IllegalStateException => error = true
			}
		}

		right.start()
		right.join()

		error must beTrue
	}

	"handle scads of conflicts" in {
		val NUM = 10
		val IT = 1

		val ref = transactional {
			val ref = new Ref[Int](0)
			!ref must beEqualTo(0)
			ref
		}

			def modify {
				for (_ <- 0 until IT) {
					ref := 1 + ref
				}
			}

		val pool = (0 until NUM).foldRight(List[Thread]()) { (i, ls) =>
			val t = thread {
				try
					transactional(modify)
				catch {
					case e => e.printStackTrace; throw e
				}
			}

			t :: ls
		}

		pool foreach { _.start() }
		pool foreach { _.join() }

		transactional {
			!ref mustEqual (IT * NUM)
		}
	}

	"propagate exceptions" in {
		var caughtException = false

		try {
			transactional {
				throw new RuntimeException("Testing")
				1
			}
		} catch {
			case _: RuntimeException => caughtException = true
		}

		caughtException mustEqual true
	}

	def thread(f: => Unit): Thread = new Thread {
		override def run() {
			f
		}
	}
}
