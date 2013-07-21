package net.fwbrasil.radon.problems

import net.fwbrasil.radon.TestRadonContext._
import org.specs2.mutable._
import scala.util.Random
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class SleepingBarber extends Specification {

    case class Customer(barber: Barber, waitingRoom: WaitingRoom) extends Thread {
        var hairOk = false
        override def run = {
            hairOk =
                transactional {
                    if (barber.isSleeping) {
                        barber.cutHair(this)
                        true
                    } else if (!waitingRoom.isFull) {
                        waitingRoom.sitDown(this)
                        true
                    } else false
                }
        }
    }

    class Chair extends Ref[Customer] {
        def isEmpty =
            super.get.isEmpty
        def sitDown(customer: Customer) = {
            if (!isEmpty)
                throw new IllegalStateException("Chair is not empty!")
            super.put(Option(customer))
        }
    }

    case class WaitingRoom(chairs: List[Chair]) {
        def isFull =
            emptyChairs.isEmpty
        def emptyChairs =
            for (chair <- chairs if (chair.isEmpty))
                yield chair
        def sitDown(customer: Customer) =
            emptyChairs.head.sitDown(customer)
        def nonEmptyChairs =
            for (chair <- chairs if (!chair.isEmpty))
                yield chair
        def hasCustomerWaiting =
            nonEmptyChairs.nonEmpty
        def pickUpACustomer = {
            val chair = nonEmptyChairs.head
            val customer = !chair
            chair.put(None)
            customer
        }
    }

    case class Barber(barberChair: Chair, waitingRoom: WaitingRoom, fStop: () => Boolean) extends Thread {
        override def run = {
            transactionalWhile(waitingRoom.hasCustomerWaiting || !isBarberChairEmpty || !fStop()) {
                if (!isBarberChairEmpty) {
                    Thread.sleep(10) // Cut hair
                    barberChair.put(None)
                } else if (waitingRoom.hasCustomerWaiting)
                    barberChair := waitingRoom.pickUpACustomer
            }
        }
        def isBarberChairEmpty =
            barberChair.isEmpty
        def isCuttingHair =
            !isBarberChairEmpty
        def isSleeping =
            isBarberChairEmpty
        def cutHair(customer: Customer) = {
            if (isCuttingHair)
                throw new IllegalStateException("The barber is alredy cutting hair!")
            barberChair := customer
        }
    }

    "SleepingBarber" should {
        "work" in {
            testSleepingBarber(10, 10)
            testSleepingBarber(10, 11)
            testSleepingBarber(10, 12)
            testSleepingBarber(100, 100)
            testSleepingBarber(10, 100)
            testSleepingBarber(100, 10)
        }
    }

    def testSleepingBarber(numberOfWaitingRoomChairs: Int, numberOfCustomers: Int) = {
        val random = new Random
        var stop = false
        val waitingRoomChairs =
            transactional {
                for (i <- 0 until numberOfWaitingRoomChairs)
                    yield new Chair
            }
        val waitingRoom =
            WaitingRoom(waitingRoomChairs.toList)
        val barber = transactional {
            Barber(new Chair, waitingRoom, () => stop)
        }
        val customers =
            for (i <- 0 until numberOfCustomers)
                yield Customer(barber, waitingRoom)

        barber.start
        for (customer <- customers) {
            customer.start
            Thread.sleep(random.nextInt(10))
        }
        customers.foreach(_.join)
        stop = true;
        barber.join
        if (numberOfWaitingRoomChairs >= numberOfCustomers + 1) {
            transactional {
                customers.forall(_.hairOk)
            } must beTrue
        }
        transactional {
            barber.isBarberChairEmpty
        } must beTrue
        transactional {
            waitingRoom.hasCustomerWaiting
        } must beFalse
    }

}