package net.fwbrasil.radon.transaction

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class NestedTransactionSpecs extends Specification {

	"A nested transaction" should {

		"see the scope of the outer transaction" in {
			val ref =
				transactional {
					new Ref(100)
				}
			transactional {
				ref := 200
				transactional(nested) {
					!ref must beEqualTo(200)
				}
			}
		}

		"be isolated" in {
			val ref =
				transactional {
					new Ref(100)
				}
			transactional {
				ref := 200
				transactional(nested) {
					ref := 300
					throw new IllegalStateException
					1
				} must throwA[IllegalStateException]
				!ref must beEqualTo(200)
			}
		}

		"have changes sent to the outer transaction after commit" in {
			val ref =
				transactional {
					new Ref(100)
				}
			transactional {
				ref := 200
				transactional(nested) {
					ref := 300
				}
				!ref must beEqualTo(300)
			}
			transactional {
				!ref must beEqualTo(300)
			}

		}

	}
}