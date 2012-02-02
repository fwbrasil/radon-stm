package net.fwbrasil.radon.transaction

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import net.fwbrasil.radon.dsl.actor._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class TransactionValidatorSpecs extends Specification {

	"Transaction validator" should {
		"validate write" in {
			"return error if ref is read by concurrent transaction" in {
				"on commit" in {
					new TransactorDsl with TwoTransactors with OneTransactorPerThread {
						val ref =
							transactional {
								new Ref(1)
							}
						actor1.startTransactionIfNotStarted
						inActor2 {
							!ref must beEqualTo(1)
						}
						actor2.markTransactionNotReadOnly
						inActor1 {
							ref := 10
						}
						actor2.commit
						actor1.commit must throwA[ConcurrentTransactionException]
					} must not beNull
				}
			}
			"not return error if ref is read by concurrent read only transaction" in {
				"during transaction" in {
					new TransactorDsl with TwoTransactors with OneTransactorPerThread {
						val ref =
							transactional {
								new Ref(1)
							}
						actor1.startTransactionIfNotStarted
						inActor2 {
							!ref must beEqualTo(1)
						}
						actor2.commit
						inActor1 {
							ref := 10
						}
						actor1.commit
						transactional {
							!ref must beEqualTo(10)
						}
					} must not beNull
				}
				"on commit" in {
					new TransactorDsl with TwoTransactors with OneTransactorPerThread {
						val ref =
							transactional {
								new Ref(1)
							}
						actor1.startTransactionIfNotStarted
						inActor2 {
							!ref must beEqualTo(1)
						}
						inActor1 {
							ref := 10
						}
						actor2.commit
						actor1.commit
						transactional {
							!ref must beEqualTo(10)
						}
					} must not beNull
				}
			}
		}

		"validate read" in {
			"return error if ref is write by concurrent transaction" in {
				"on commit" in {
					new TransactorDsl with TwoTransactors with OneTransactorPerThread {
						val ref =
							transactional {
								new Ref(1)
							}
						actor1.startTransactionIfNotStarted
						inActor2 {
							ref := 10
						}
						inActor1 {
							!ref
						}
						actor2.markTransactionNotReadOnly
						actor2.commit
						actor1.markTransactionNotReadOnly
						actor1.commit must throwA[ConcurrentTransactionException]
					} must not beNull
				}
			}
			"not return error if concurrent transaction read the ref" in {
				"during transaction" in {
					new TransactorDsl with TwoTransactors with OneTransactorPerThread {
						val ref =
							transactional {
								new Ref(1)
							}
						actor1.startTransactionIfNotStarted
						inActor2 {
							!ref
						}
						actor2.markTransactionNotReadOnly
						actor2.commit
						inActor1 {
							!ref
						} must beEqualTo(1)
					} must not beNull
				}
				"on commit" in {
					new TransactorDsl with TwoTransactors with OneTransactorPerThread {
						val ref =
							transactional {
								new Ref(1)
							}
						actor1.startTransactionIfNotStarted
						inActor2 {
							!ref
						}
						inActor1 {
							!ref
						} must beEqualTo(1)
						actor2.markTransactionNotReadOnly
						actor2.commit
						actor1.commit
					} must not beNull
				}
			}
		}
		"validate read and write" in {
			"return error if ref is read and write by concurrent transaction" in {
				"under stress" in {
					new ActorDsl with ManyActors {
						override lazy val actorsPoolSize = 10
						val ref =
							transactional {
								new Ref(0)
							}
						inParallelActors {
							transactional {
								ref := ref + 1
							}
						}
						transactional {
							!ref
						} must beEqualTo(actorsPoolSize)
					} must not beNull
				}
			}
		}

		"validate destroyed refs" in {
			"during transaction" in {
				transactional {
					val ref = new Ref(10)
					ref := 200
					ref.destroy
					!ref must throwA[IllegalStateException]
					(ref := 300) must throwA[IllegalStateException]
					ref.destroy must throwA[IllegalStateException]
				}
			}

		}
	}

}