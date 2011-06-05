package net.fwbrasil.radon

import net.fwbrasil.radon.dsl.actor._
import net.fwbrasil.radon.TestRadonContext._
import org.specs2.mutable._
import org.junit.runner._
import org.specs2.runner._

class Pessoa(val nome: Ref[String])

@RunWith(classOf[JUnitRunner])
class ACITest extends Specification {

	"An atomic transaction" should {
		"do all" in {
			val ref = transactional {
				new Ref(100)
			}
			transactional {
				ref := ref + 200
			}
			transactional {
				!ref must beEqualTo(300)
			}
		}
		"or do nothing" in {
			val ref = transactional {
				new Ref(100)
			}
			try {
				transactional {
					ref := ref + 200 
					throw new IllegalStateException
					ref := ref + 200
				}
			} catch {case e => }
				
			transactional {
				!ref must beEqualTo(100)
			}
		}
	}

	"A consistent transaction" should {
		"throw exception if data changed after read" in {
			new TransactorDsl with TwoTransactors with OneTransactorPerThread {
				val ref = transactional {
					new Ref(100)
				}
				inActor1 {
					ref := 300 + ref
				}
				inActor2 {
					ref := 200
				}
				inMainActor {
					actor2.commit
				}
				inMainActor {
					actor1.commit must throwA[ConcurrentTransactionException]
				}
			} must not beNull
		}
	}
}