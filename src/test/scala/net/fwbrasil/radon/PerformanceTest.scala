package net.fwbrasil.radon

import org.scalameter.api._
import net.fwbrasil.radon.TestRadonContext._

object RadonPerformanceTest
        extends PerformanceTest.Regression {

    def persistor = new SerializationPersistor

    val sizes: Gen[Int] =
        Gen.range("size")(100, 10000, 100)

    val ranges: Gen[Range] = for {
        size <- sizes
    } yield 0 until size

    performance of "Ref" in {
        measure method "empty constructor" in {
            using(ranges) config (
                exec.independentSamples -> 1) in {
                    range =>
                        transactional {
                            for (i <- range)
                                new Ref[Int]
                        }
                }
        }
    }

}

