package net.fwbrasil.radon.ref

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon.ConcurrentTransactionException
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class RefSpecs extends Specification {

    "A Ref" should {
        "deal with values" in {
            "(djspiewak) type-system magic should compile" in transactional {
                val ref = new Ref(42)

                // covariance
                val subSource: Source[Int] = ref
                val superSource: Source[AnyVal] = ref

                // contravariance
                val objRef = new Ref[Any]
                val subSink: Sink[Any] = objRef
                val superSink: Sink[String] = objRef
                ok
            }

            "set the value when crated" in transactional {
                val ref = new Ref(100)
                !ref must beEqualTo(100)
                ref.get must beSome(100)
            }

            "set null if is created without a value" in transactional {
                val ref = new Ref[Int]()
                !ref must beEqualTo(0)
            }

            "set null after is created with a value" in transactional {
                val ref2 = new Ref(100)
                ref2.put(None)
                !ref2 must beEqualTo(0)
                ref2.get must beNone
            }

            "set the value if it wasn't null" in transactional {
                val ref = new Ref[Int](10)
                ref := 100
                !ref must beEqualTo(100)
                ref.get must beEqualTo(Some(100))

                ref.put(Option(200))
                !ref must beEqualTo(200)
                ref.get must beEqualTo(Some(200))
            }

            "set the value if it was null" in transactional {
                val ref = new Ref[Int]()
                ref := 100
                !ref must beEqualTo(100)
                ref.get must beEqualTo(Some(100))

                ref.put(Option(200))
                !ref must beEqualTo(200)
                ref.get must beEqualTo(Some(200))
            }
        }
        "retain the creation transaction" in {
            val transaction = new Transaction
            val ref =
                transactional(transaction) {
                    new Ref(100)
                }
            ref.creationTransactionId must beEqualTo(transaction.transactionId)
        }
        "detect if it is creating" in transactional {
            val ref = new Ref(100)
            ref.isCreating must beTrue
        }
        "deal with timestamps" in {
            "at creation" in transactional {
                val ref = new Ref(100)
                ref.readTimestamp must beEqualTo(0)
                ref.readTimestamp must beEqualTo(0)
            }
            "after creation" in {
                val transaction = new Transaction
                val ref =
                    transactional(transaction) {
                        new Ref(100)
                    }
                transaction.commit
                ref.readTimestamp must beEqualTo(0)
                ref.writeTimestamp must beLessThan(transactionClock.tick)
            }
            "after read" in {
                val transaction1 = new Transaction
                val transaction2 = new Transaction
                val ref =
                    transactional(transaction1) {
                        new Ref(100)
                    }
                transaction1.commit
                val transaction1AfterEndTimestamp = transactionClock.tick
                transactional(transaction2) {
                    !ref
                }
                val transaction2StartTimestamp = transaction2.startTimestamp
                transaction2.commit
                ref.readTimestamp must beEqualTo(transaction2StartTimestamp)
                ref.writeTimestamp must beLessThan(transaction1AfterEndTimestamp)
            }
            "after write" in {
                val transaction1 = new Transaction
                val transaction2 = new Transaction
                val ref =
                    transactional(transaction1) {
                        new Ref(100)
                    }
                transaction1.commit
                transactional(transaction2) {
                    ref := 200
                }
                transaction2.commit
                ref.readTimestamp must beEqualTo(0)
                ref.writeTimestamp must beLessThan(transactionClock.tick)
            }
            "after read and write" in {
                val transaction1 = new Transaction
                val transaction2 = new Transaction
                val ref =
                    transactional(transaction1) {
                        new Ref(100)
                    }
                transaction1.commit
                transactional(transaction2) {
                    ref := !ref + 200
                }
                val transaction2StartTimestamp = transaction2.startTimestamp
                transaction2.commit
                ref.readTimestamp must beEqualTo(transaction2StartTimestamp)
                ref.writeTimestamp must beLessThan(transactionClock.tick)
            }
        }
        "be destoyed" in {
            "one transaction" in transactional {
                val ref = new Ref(100)
                ref.destroy
                ref.isDestroyed must beTrue
            }
            "different transanctions" in {
                val ref =
                    transactional {
                        val ref = new Ref(100)
                        ref.isDestroyed must beFalse
                        ref.get must beEqualTo(Some(100))
                        ref.put(Option(200))
                        ref
                    }
                transactional {
                    ref.destroy
                    ref.isDestroyed must beTrue
                }
                transactional {
                    ref.isDestroyed must beTrue
                }
            }
            "concurrent transactions" in {
                "validate creation" in {
                    val t1 = new Transaction
                    val t2 = new Transaction
                    val ref =
                        transactional(t1) {
                            new Ref(100)
                        }
                    transactional(t2) {
                        ref.destroy
                    }
                    t2.commit must throwA[ConcurrentTransactionException]
                }

                "validate concurrent" in {
                    val t1 = new Transaction
                    val t2 = new Transaction
                    val ref =
                        transactional {
                            new Ref(100)
                        }
                    transactional(t1) {
                        ref := 300
                    }
                    transactional(t2) {
                        ref.destroy
                    }
                    t2.commit
                    t1.commit must throwA[ConcurrentTransactionException]
                }
            }

        }
        "return the original value" in {
            val ref = transactional(new Ref(0))
            transactional {
                ref.getOriginalValue === Some(0)
                ref := 1
                ref.getOriginalValue === Some(0)
                ref.get === Some(1)
            }
        }
    }

}