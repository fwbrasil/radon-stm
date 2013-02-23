package net.fwbrasil.radon.transaction

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import net.fwbrasil.radon.dsl.actor._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class TransactionManagerSpecs extends Specification {

    "TransactionManager" should {
        "activating a transaction" in {
            "activate only for the thread" in {
                new ActorDsl with ManyActors with OneActorPerThread {
                    val manager = new TransactionManager()
                    inParallelActors {
                        val transaction = Option(new Transaction)
                        manager.getActiveTransaction must beNone
                        manager.activate(transaction)
                        manager.getActiveTransaction must beEqualTo(transaction)
                    }
                } must not beNull
            }
            "return error if there is another transaction active" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.activate(transaction)
                val transaction2 = Option(new Transaction)
                manager.activate(transaction2) must throwA[IllegalStateException]
            }
            "return error if is activating none and exists another active transaction" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.activate(transaction)
                manager.activate(None) must throwA[IllegalStateException]
            }
            "not return error if is activating the same transaction" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.activate(transaction)
                manager.activate(transaction)
                manager.getActiveTransaction must beEqualTo(transaction)
            }
        }
        "desactivating a transaction" in {
            "desactivate if transaction is active" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.activate(transaction)
                manager.getActiveTransaction must beEqualTo(transaction)
                manager.deactivate(transaction)
                manager.getActiveTransaction must beNone
            }
            "return error if transaction is not active" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.deactivate(transaction) must throwA[IllegalStateException]
            }
            "return error when trying to deactivate None" in {
                val manager = new TransactionManager()
                manager.deactivate(None)
                true must beTrue
            }
            "return error when trying to deactivate None with an active transaction" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.activate(transaction)
                manager.getActiveTransaction must beEqualTo(transaction)
                manager.deactivate(None) must throwA[IllegalStateException]
            }
        }
        "returning the required active transaction" in {
            "return the active transaction" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.activate(transaction)
                manager.getRequiredActiveTransaction must beEqualTo(transaction.get)
            }
            "return error if there i'snt an active transaction" in {
                val manager = new TransactionManager()
                val transaction = Option(new Transaction)
                manager.getActiveTransaction must beNone
                manager.getRequiredActiveTransaction must throwA[RequiredTransactionException]
            }
        }

        "running in transaction" in {
            "with some transaction" in {
                val manager = new TransactionManager()
                val transaction = new Transaction
                manager.getActiveTransaction must beNone
                manager.runInTransaction(transaction) {
                    manager.getRequiredActiveTransaction must beEqualTo(transaction)
                    1
                } must beEqualTo(1)
                manager.getActiveTransaction must beNone
            }
            "with some error" in {
                val manager = new TransactionManager()
                val transaction = new Transaction
                manager.getActiveTransaction must beNone
                manager.runInTransaction(transaction) {
                    manager.getRequiredActiveTransaction must beEqualTo(transaction)
                    throw new IllegalStateException
                    1
                } must throwA[IllegalStateException]
                transaction.startTimestamp must beEqualTo(0)
                manager.getActiveTransaction must beNone
            }
        }

        "running in transaction with retry" in {
            "happy day" in {
                val manager = new TransactionManager()
                val transaction = new Transaction
                val ref =
                    transactional {
                        new Ref(100)
                    }
                manager.runInTransactionWithRetry(transaction) {
                    manager.getRequiredActiveTransaction must beEqualTo(transaction)
                    transactional {
                        ref := 200
                    }
                    1
                } must beEqualTo(1)
                transactional {
                    !ref
                } must beEqualTo(200)
                transaction.startTimestamp must beEqualTo(0)
                manager.getActiveTransaction must beNone
            }
            "one retry" in {
                val manager = new TransactionManager()
                val transaction = new Transaction
                var error = true
                val ref =
                    transactional {
                        new Ref(100)
                    }
                manager.runInTransactionWithRetry(transaction) {
                    manager.getRequiredActiveTransaction must beEqualTo(transaction)
                    transactional {
                        ref := 200
                    }
                    if (error) {
                        error = false
                        throw new ConcurrentTransactionException(List())
                    }
                    1
                } must beEqualTo(1)
                transactional {
                    !ref
                } must beEqualTo(200)
                transaction.startTimestamp must beEqualTo(0)
                manager.getActiveTransaction must beNone
            }
            "retry limit" in {
                val manager = new TransactionManager()
                val transaction = new Transaction
                val ref =
                    transactional {
                        new Ref(100)
                    }
                manager.runInTransactionWithRetry(transaction) {
                    manager.getRequiredActiveTransaction must beEqualTo(transaction)
                    transactional(transaction) {
                        ref := 200
                        throw new ConcurrentTransactionException(List())
                        1
                    }
                } must throwA[RetryLimitTransactionException]
                transactional {
                    !ref
                } must beEqualTo(100)
                transaction.startTimestamp must beEqualTo(0)
                manager.getActiveTransaction must beNone
            }
            "RetryWithWriteTransactionException" in {
                val manager = new TransactionManager()
                val transaction = new Transaction
                val ref =
                    transactional {
                        new Ref(100)
                    }
                manager.runInTransactionWithRetry(transaction) {
                    manager.getRequiredActiveTransaction must beEqualTo(transaction)
                    transactional {
                        ref := 200
                    }
                    if (!transaction.isRetryWithWrite)
                        throw new RetryWithWriteTransactionException(List())
                    1
                } must beEqualTo(1)
                transactional {
                    !ref
                } must beEqualTo(200)
                transaction.startTimestamp must beEqualTo(0)
                manager.getActiveTransaction must beNone
            }
        }
    }

}