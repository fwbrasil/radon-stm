package net.fwbrasil.radon.util
 
import scala.collection._

object ThreadUtil {

	def runInNewThread(f: => Unit): Thread = {
		val thread = new Thread {
			override def run() {
				f
			}
		}
		thread.start
		thread
	}
	
	def runInNewThreadAndWaitForPark(parking: ParkingLot)(f: => Unit): Thread = {
		val thread = runInNewThread(f)
		parking.waitForPark(thread)
		thread
	}
	
	class ParkingLot {
		
		private[this] val parkedThreads = 
			new mutable.HashSet[Thread]() 
				with mutable.SynchronizedSet[Thread]
		
		def isParked(threads: Thread*): Boolean = 
			threads.forall(parkedThreads.contains(_))
		
		def park = {
			val currentThread = Thread.currentThread
			parkedThreads += currentThread
			while(isParked(currentThread)) 
				Thread.sleep(10)
		}
		
		def unpark(threads: Thread*) =
			threads.foreach(parkedThreads.remove(_))
		
		def unparkAndJoin(threads: Thread*) = {
			unpark(threads:_*)
			threads.foreach(_.join)
		}
		
		def unparkAll =
			unpark(parkedThreads.toArray[Thread]:_*)
			
		def unparkAndJoinAll =
			unparkAndJoin(parkedThreads.toArray[Thread]:_*)
				
		def waitForPark(threads: Thread*): Unit =
			while(!threads.forall(parkedThreads.contains(_)))
				Thread.sleep(10)
				
		def waitForParkAndUnpark(threads: Thread*): Unit = {
			waitForPark(threads:_*)
			unpark(threads:_*)
		}
	}
}