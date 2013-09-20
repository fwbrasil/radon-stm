package net.fwbrasil.radon.ref

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon.ConcurrentTransactionException
import org.junit.runner._
import org.specs2.runner._
import net.fwbrasil.radon.transaction.NestedTransaction

@RunWith(classOf[JUnitRunner])
class RefListenerSpecs extends Specification {

    "A Ref listener" should {
        "receive put notifications" in transactional {
            val ref = new Ref(0)
            var notified: Option[Int] = None
            val listener =
                new RefListener[Int] {
                    override def notifyPut(ref: Ref[Int], obj: Option[Int]) =
                        notified = obj
                }
            ref.addWeakListener(listener)
            def testValue(value: Option[Int]) = {
                ref.put(value)
                notified === value
            }
            testValue(Some(1))
            testValue(None)
        }

        "receive get notifications" in transactional {
            val ref = new Ref(0)
            var notified = false
            val listener =
                new RefListener[Int] {
                    override def notifyGet(ref: Ref[Int]) =
                        notified = true
                }
            ref.addWeakListener(listener)
            ref.get
            notified must beTrue
        }

        "run notifications inside a nested transaction" in transactional {
            val ref = new Ref(0)
            val outerTransaction = transactionManager.getRequiredActiveTransaction
            val listener =
                new RefListener[Int] {
                    override def notifyGet(ref: Ref[Int]) =
                        verifyNestedTransaction
                    override def notifyPut(ref: Ref[Int], obj: Option[Int]) =
                        verifyNestedTransaction
                    private def verifyNestedTransaction = {
                        transactionManager.getRequiredActiveTransaction match {
                            case nested: NestedTransaction =>
                                nested.parent === outerTransaction
                            case other =>
                                failure("shoud be a nested transaction")
                        }
                    }
                }
            ref.addWeakListener(listener)
            ref.put(Some(1))
            ref.get
            ok
        }
        
    }

}