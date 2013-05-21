package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.RadonContext
import org.specs2.mutable._
import net.fwbrasil.radon._
import org.junit.runner._
import org.specs2.runner._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class DurableTransactionSpecs extends Specification {

    class DurableTestContext extends RadonContext {
        var f: (Transaction) => Unit = _
        override def makeDurable(transaction: Transaction) =
            f(transaction)
        override def makeDurableAsync(transaction: Transaction)(implicit ectx: ExecutionContext) =
            Future(f(transaction))
    }

    "Durable transaction" should
        test(_.commit)

    "Async durable transaction" should {

        import scala.concurrent.ExecutionContext.Implicits.global
        
        "accept a transactional future chain" in {
            val ctx = new DurableTestContext
            ctx.f = (t: Transaction) => {}
            import ctx._
            val ref =
                transactional {
                    new Ref(100)
                }
            val f =
                asyncTransactionalFuture {
                    implicit ctx =>
                        Future(!ref)(ctx)
                }
            val res = Await.result(f, Duration.Inf)
            res === 100
            ok
        }

        test(t => Await.result(t.asyncCommit, Duration.Inf))
    }

    def test(commitFunction: Transaction => Unit) = {
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
            commitFunction(transaction)
            ok
        }

        "do not make durable reads" in {
            val ctx = new DurableTestContext
            import ctx._
            val transaction = new Transaction
            ctx.f =
                (t: Transaction) => {}
            val ref =
                transactional {
                    new Ref(100)
                }
            transactional {
                ref.get
            }
            ctx.f =
                (t: Transaction) => {
                    t must beEqualTo(transaction)
                    t.assignments must beEmpty
                }
            commitFunction(transaction)
            ok
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
            commitFunction(transaction)
            ok
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
            commitFunction(transaction) must throwA[IllegalStateException]
            ctx.f = (t: Transaction) => {}
            transactional {
                ref.isDestroyed must beTrue
            }
            ok
        }
        "do not make durable rollback" in {
            val ctx = new DurableTestContext
            import ctx._
            val transaction = new Transaction
            val ref =
                transactional(transaction) {
                    new Ref(100)
                }
            ctx.f =
                (t: Transaction) => {
                    throw new IllegalStateException("should not get here.")
                }
            transaction.rollback
            ctx.f = (t: Transaction) => {}
            transactional {
                val destroyed = ref.isDestroyed
                destroyed must beTrue
            }
            ok
        }
        "do not make durable rollback if an error occurs at nested transaction inside makeDurable" in {
            val ctx = new DurableTestContext
            import ctx._
            val transaction = new Transaction
            val ref =
                transactional(transaction) {
                    new Ref(100)
                }
            ctx.f =
                (t: Transaction) => {
                    val nestedTransaction = new NestedTransaction(transaction)
                    transactional(nestedTransaction) {
                        throw new IllegalStateException
                    }
                }
            commitFunction(transaction) must throwA[IllegalStateException]
            ctx.f = (t: Transaction) => {}
            transactional {
                ref.isDestroyed must beTrue
            }
            ok
        }
    }

}