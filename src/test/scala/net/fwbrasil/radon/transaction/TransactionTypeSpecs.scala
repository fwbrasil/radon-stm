package net.fwbrasil.radon.transaction

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import net.fwbrasil.radon.dsl.actor._
import org.junit.runner._
import org.specs2.runner._
import scala.util.Try
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class TransactionTypeSpecs extends Specification {

    "Read only transactions" should {

        "accept reads" in {
            val ref = transactional(new Ref(0))
            transactional(readOnly) {
                ref.get
            } must beSome(0)

        }
        "not accept writes" in {
            val ref = transactional(new Ref(0))
            transactional(readOnly) {
                ref := 0
            } must throwA[IllegalStateException]
            transactional {
                ref.get
            } must beSome(0)
        }
        "not accept mixed reads/writes" in {
            val ref = transactional(new Ref(0))
            transactional(readOnly, required) {
                ref.get
                ref := 0
            } must throwA[IllegalStateException]
            transactional {
                ref.get
            } must beSome(0)
        }
        "not accept ref creation" in {
            transactional(readOnly, required) {
                new Ref(0)
            } must throwA[IllegalStateException]
        }
        "not accept writes on a readOnly nested transaction" in {
            transactional {
            	val ref = new Ref(0)
                transactional(readOnly, nested) {
                    ref := 1
                } must throwA[IllegalStateException]
                ref.get
            } must beSome(0)
        }
    }
    
    "Read-write transactions" should {
        
        "accept reads and writes" in {
            val ref = transactional(new Ref(0))
            transactional(readWrite) {
                ref := 1
                ref.get
            } must beSome(1)
        }
    }

}