package net.fwbrasil.radon.problems

import org.specs2.mutable._
import net.fwbrasil.radon.TestRadonContext._
import net.fwbrasil.radon._
import net.fwbrasil.radon.dsl.actor._
import scala.util.Random
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class CigaretteSmokers extends Specification {

	class Ingredient
	object tobaco extends Ingredient {
		override def toString = "tobaco"
	}
	object paper extends Ingredient {
		override def toString = "paper"
	}
	object aMatch extends Ingredient {
		override def toString = "match"
	}

	type Table = Ref[List[Ingredient]]

	case class Smoker(table: Table, ingredient: Ingredient, var units: Ref[Int]) extends Thread {
		setName("Smoker:" + ingredient)
		override def run = {
			transactionalWhile(hasUnits) {
				val ingredients = !table
				if (!ingredients.isEmpty && !ingredients.contains(ingredient)) {
					table := List()
					takeUnit
					Thread.sleep(10) // Smoke
				} else if (hasUnits)
					retry(table)
			}
		}
		def hasUnits =
			units > 0
		def takeUnit = {
			units := units - 1
			ingredient
		}
	}

	case class Arbiter(table: Table, smokers: List[Smoker]) extends Thread {
		val random = new Random
		setName("Arbiter")
		override def run = {
			transactionalWhile(smokersHasUnits) {
				if (table.isEmpty) {
					val randomSmokers = random.shuffle(smokers)
					table := List(randomSmokers(0).takeUnit, randomSmokers(1).takeUnit)
				} else if (smokersHasUnits)
					retry(table)
			}
		}
		def smokersHasUnits =
			smokers.forall(_.units > 0)

	}

	"CigaretteSmokers" should {
		"work" in {
			val table = transactional {
				new Table(List())
			}
			val units = 100
			val smokers = transactional {
				List(Smoker(table, tobaco, units),
					Smoker(table, paper, units),
					Smoker(table, aMatch, units))
			}
			val arbiter =
				Arbiter(table, smokers)
			arbiter.start
			smokers.foreach(_.start)
			arbiter.join
			smokers.foreach(_.join)
			val hasUnits =
				transactional {
					for (smoker <- smokers)
						yield smoker.hasUnits
				}
			hasUnits.foreach(_ must beFalse)
			true must beTrue
		}
	}
}