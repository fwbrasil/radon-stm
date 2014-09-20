 package net.fwbrasil.radon.problems

 import scala.util.Random
 import org.junit.runner._
 import org.specs2.runner._
 import org.specs2.mutable._
 import net.fwbrasil.radon.TestRadonContext._
 import Thread._
 import scala.concurrent.Await
 import scala.concurrent.Future
 import scala.concurrent.duration.Duration

 @RunWith(classOf[JUnitRunner])
 class AsyncBankFrenzy extends Specification {

     case class Transfer(amount: Int, from: Account, to: Account)

     type Account = Ref[Int]

     val log =
         transactional {
             new Ref(List[Transfer]())
         }

     val fees =
         transactional {
             new Account
         }

     "Bank frenzy" should {
         "work" in {

             val business1 = transactional {
                 new Account(15000)
             }
             val business2 = transactional {
                 new Account(20000)
             }
             val business3 = transactional {
                 new Account(50000)
             }

             val people =
                 transactional {
                     for (
                         i <- 0 until 200
                     ) yield new Account(100)
                 }

             val market = List(business1, business2, business3, fees) ++ people.toList
             var running = true

             val businessActor = thread {
                 def exec: Unit =
                     asyncTransactional {
                         transfer(250, business1, business2)
                     }.map { _ =>
                         if (running) {
                             sleep(200)
                             exec
                         }
                     }
                 exec
             }

             def marketValue = transactional(sum(market))

             val startValue = marketValue

             businessActor.start()
             val secActor = thread {
                 def exec: Unit =
                     asyncTransactional {
                         marketValue mustEqual startValue
                     }.map { _ =>
                         if (running) {
                             sleep(10)
                             exec
                         }
                     }
                 exec
             }
             secActor.start()

             val peopleFutures =
                 for {
                     i <- 0 until people.length
                     p = people(i)
                 } yield {
                     asyncTransactional(transfer(50, p, business3)).flatMap(_ =>
                         asyncTransactional(transfer(i * 10, p, business1))).flatMap(_ =>
                         asyncTransactional(transfer(i * 3, business2, p)))
                 }

             Await.result(Future.sequence(peopleFutures), Duration.Inf)

             running = false

             businessActor.join()
             secActor.join()

             marketValue mustEqual startValue
             transactional {
                 val logSize = !log.size
                 val minExcpected = people.size * 3
                 logSize > minExcpected
             } must beTrue

             true must beTrue

         }

     }
     def transfer(amount: Int, from: Account, to: Account) {
         log := !log ++ List(Transfer(amount, from, to))

         val less = scala.math.round(amount * 0.075).toInt

         from := from - amount
         to := to + (amount - less)
         fees := fees + less
     }

     def thread(f: => Unit) = new Thread {
         override def run() {
             f
         }
     }

     def sum(portfolio: List[Account]) = {
         portfolio.foldRight(0: Int) { _ + _ }
     }

 }