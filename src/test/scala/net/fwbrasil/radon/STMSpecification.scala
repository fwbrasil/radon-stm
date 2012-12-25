package net.fwbrasil.radon

import org.specs2.mutable._

import net.fwbrasil.radon.TestRadonContext._

import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class STMSpecification extends Specification {

	"The value of a ref" should {
		"be set to value in creation" in transactional {
			val ref = new Ref(100)
			!ref must beEqualTo(100)
		}
		"be set to null in creation" in transactional {
			val ref = new Ref[Long](null)
			!ref must beEqualTo(0)
		}
		"be modified to value" in transactional {
			val ref = new Ref(100)
			ref := 200
			!ref must beEqualTo(200)
		}
		"be modified to null by put" in transactional {
			val ref = new Ref(100)
			ref.put(null)
			!ref mustEqual 0
		}
	}

	"Whithout transaction a ref" should {
		"not be created" in {
			new Ref[Long](100) must throwA[RequiredTransactionException]
		}
		"not be created with null" in {
			new Ref[Long](null) must throwA[RequiredTransactionException]
			new Ref[Long] must throwA[RequiredTransactionException]
		}
		"not be altered" in {
			val ref = transactional {
				new Ref[Long](100)
			}
			ref must not be null
			(ref := 200) must throwA[RequiredTransactionException]
		}
		"not be altered to null" in {
			val ref = transactional {
				new Ref[Long](100)
			}
			ref must not be null
			(ref := null.asInstanceOf[Long]) must throwA[RequiredTransactionException]
		}
		"not be read" in {
			val ref = transactional {
				new Ref[Long](100)
			}
			!ref must throwA[RequiredTransactionException]
			ref.get must throwA[RequiredTransactionException]
			(ref + 1) must throwA[RequiredTransactionException]
		}
	}

	"A transaction" should {
		"be atomic" in {
			val ref = transactional {
				new Ref(100)
			}
			try {
				transactional {
					ref := 200
					throw new IllegalStateException
					1
				}
			} catch { case _ => }
			transactional {
				!ref must beEqualTo(100)
			}
		}
	}
	"Concurrent transactions in the same thread" should {
		"be isolated" in {
			val ref = transactional {
				new Ref(100)
			}
			val t1 = new Transaction
			transactional(t1) {
				ref := 200
			}
			val t2 = new Transaction
			transactional(t2) {
				!ref must beEqualTo(100)
			}
			t1.commit
			t2.commit
			transactional {
				!ref must beEqualTo(200)
			}
		}
	}

}