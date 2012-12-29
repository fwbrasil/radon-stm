package net.fwbrasil.radon

import org.scalameter.api._
import net.fwbrasil.radon.TestRadonContext._

object RadonPerformanceTest
		extends PerformanceTest.Regression {

	def persistor = new SerializationPersistor

	val nums: Gen[Int] =
		Gen.range("num")(0, 100000, 100)

	performance of "Ref" in {
		measure method "empty constructor" in {
			using(nums) in {
				_ => transactional(new Ref[Int])
			}
		}
	}

}

