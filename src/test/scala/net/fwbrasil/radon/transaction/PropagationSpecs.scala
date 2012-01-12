package net.fwbrasil.radon.transaction

import org.specs2.mutable._ 
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class PropagationSpecs extends Specification {
	
	"Required transaction propagation" should {
		
		"accept a transaction and use it" in {
			acceptATransactionAndUseIt(required)
		}
		
		"create a new transaction if actual is None" in {
			transactional(required) {
				new Ref(100)
			} must not be null
		}
	}
	
	"Mandatory transaction propagation" should {
		
		"accept a transaction and use it" in 
			acceptATransactionAndUseIt(mandatory)
		
		"return error if actual is None" in {
			transactional(mandatory) {
				new Ref(100)
			} must throwA[RequiredTransactionException]
		}
	}
	
	"Never transaction propagation" should {
		
		"run without a transaction" in 
			runWithoutATransaction(never)
		
		"do not accept a transaction" in {
			val transaction = new Transaction
			transactional(transaction, never) {
				new Ref(new Object)
			} must throwA[NotSupportedTransactionException]
		}
	}
	
	"NotSupported transaction propagation" should {
		
		"accept a transaction and desactivate it" in {
			val transaction = new Transaction
			transactional(transaction) {
				transactional(notSupported) {
					new Ref(new Object) must throwA[RequiredTransactionException]
				}
			}
			transaction.refsAssignments.isEmpty must beTrue
		}
		
		"run without a transaction" in
			runWithoutATransaction(notSupported)
			
	}
	
	"RequiresNew transaction propagation" should {
		
		"accept a transaction, desactivate it and run with a new one" in {
			val transaction = new Transaction
			val ref = 
				transactional(transaction) {
					transactional(requiresNew) {
						new Ref(new Object)
					}
				}
			transaction.refsAssignments.isEmpty must beTrue
		}
		
		"run without a transaction and create a new one" in {
			transactional(None, requiresNew) {
				new Ref(new Object)
			} must not beNull
		}
	}
	
	"Supports transaction propagation" should {
		
		"run without a transaction" in
			runWithoutATransaction(supports)
		
		"accept a transaction and use it" in 
			acceptATransactionAndUseIt(supports)
	}
	
	private[this] def acceptATransactionAndUseIt(propagation: Propagation) = {
		val transaction = new Transaction
		val ref = 
			transactional(transaction, propagation) {
				new Ref(new Object)
			}
		transaction.refsAssignments.first._1 must beEqualTo(ref)
		transaction.commit
		true must beTrue
	}
	
	
	private[this] def runWithoutATransaction(propagation: Propagation) =
		transactional(None, propagation) {
			new Ref(new Object) must throwA[RequiredTransactionException]
		}
		
}