package net.fwbrasil.radon.transaction

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import org.junit.runner._
import org.specs2.runner._
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class TransactionAttachmentsSpecs extends Specification {

    "The transaction attachemnts" should {

        "be empty after commit" in {
            val transaction = new Transaction
            transaction.attachments += 1
            transaction.attachments === ListBuffer(1)
            transaction.commit
            transaction.attachments must beEmpty
        }

    }

}