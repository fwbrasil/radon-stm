package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.RadonContext
import org.specs2.mutable._
import net.fwbrasil.radon._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class DurableTransactionSpecs extends Specification {

	class DurableTestContext extends RadonContext {
		var f: (Transaction) => Unit = _
		override def makeDurable(transaction: Transaction) =
			f(transaction)
	}

	"Durable transaction" should {
		"make durable writes" in {
			val ctx = new DurableTestContext
			import ctx._
			val transaction = new Transaction
			val ref =
				transactional(transaction) {
					new Ref(100)
				}
			ctx.f =
				(t: Transaction) => {
					t must beEqualTo(transaction)
					t.assignments must beEqualTo(List((ref, Some(100), false)))
				}
			transaction.commit
			true must beTrue
		}

		"do not make durable transient writes" in {
			val ctx = new DurableTestContext
			import ctx._
			val transaction = new Transaction(true)
			val ref =
				transactional(transaction) {
					new Ref(100)
				}
			ctx.f =
				(t: Transaction) => {
					throw new IllegalStateException("don't make durable transient writes")
				}
			transaction.commit
			true must beTrue
		}

		"rollback when makeDurable throws exception" in {
			val ctx = new DurableTestContext
			import ctx._
			val transaction = new Transaction
			val ref =
				transactional(transaction) {
					new Ref(100)
				}
			ctx.f =
				(t: Transaction) => {
					throw new IllegalStateException("error in makeDurable")
				}
			transaction.commit must throwA[IllegalStateException]
			ctx.f = (t: Transaction) => {}
			transactional {
				ref.isDestroyed must beTrue
			}
			true must beTrue
		}
	}

}