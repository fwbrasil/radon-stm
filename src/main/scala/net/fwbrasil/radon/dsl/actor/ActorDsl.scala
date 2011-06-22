package net.fwbrasil.radon.dsl.actor

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.Futures._

abstract class ActorMessage[A]
case class ExecuteMessage[A](f: () => A) extends ActorMessage[A]
case class StopMessage() extends ActorMessage[Unit]
case class ExceptionMessage[A](ex: Exception) extends ActorMessage[A]
case class OkMessage[A](ret: A) extends ActorMessage[A]

class ExecutorActor(val oneActorPerThread: Boolean) extends Actor {
	start
	def act {
		doInLoop {
		    receive {
				case execute: ExecuteMessage[_] => 
					processExecuteMessage(execute)
				case stop: StopMessage => 
					reply(OkMessage())
					exit
				case other: ActorMessage[_] =>
					processOtherMessage(other)
		    }
		}
	}
	
	def processExecuteMessage(execute: ExecuteMessage[_]) =
		processWithReply(execute.f)
	
	def processWithReply[A](f: () => A) = 
		try {
			reply(OkMessage(f()))
		}
		catch {
			case ex: Exception => reply(ExceptionMessage(ex))
		}
		
	def processOtherMessage[A](execute: ActorMessage[A]) =
		reply(ExceptionMessage(new UnsupportedOperationException))
	
	def doInLoop[A](f: => A) =
		if(oneActorPerThread) 
			while(true) {
				f
			}
		else loop {
				f
			}
	def execute[A](f: => A) =
		syncExec(ExecuteMessage(() => f))
	
	def executeFuture[A](f: => A) =
		this !! ExecuteMessage(() => f)
		
	def stop =
		syncExec(StopMessage())
		
	def syncExec[A](message: ActorMessage[A]): A = {
		ExecutorActor.processReturn(this !? message)
	}
	
}
object ExecutorActor {
	def processReturn[A](ret: Any): A =
		ret match {
			case exm: ExceptionMessage[_] => 
				throw exm.ex
			case ok: OkMessage[A] =>
				ok.ret
		}
}

abstract class AbstractActorDsl[E <: ExecutorActor] {
	lazy val oneActorPerThread: Boolean = false
	val mainActor = newActor
	def newActor: E
	def inMainActor[A](f: => A): A =
		mainActor.execute(f)
	def stopActors: Unit = 
		mainActor.stop
}

class ActorDsl extends AbstractActorDsl[ExecutorActor] {
	def newActor = new ExecutorActor(oneActorPerThread)
}

trait AbstractOneActorPerThread[E <: ExecutorActor] extends AbstractActorDsl[E] {
	override lazy val oneActorPerThread = true
}

trait OneActorPerThread extends AbstractOneActorPerThread[ExecutorActor]

trait AbstractManyActors[E <: ExecutorActor] extends AbstractActorDsl[E] {
	lazy val actorsPoolSize = 4
	val actors = 
		(for(_ <- 0 until actorsPoolSize) 
			yield newActor).toList
	def inActors[A](f: => A): List[A] =
		for(executor <- actors)
			yield executor.execute(f)
	def inParallelActors[A](f: => A): List[A] = {
		var haveToWait = true
		def function =
			 {
				while(haveToWait) 
					Thread.sleep(5)
				f
			}
		val futures =
			for(executor <- actors)
				yield executor.executeFuture(function)
		haveToWait = false
		for(future <- futures)
			yield future() match {
				case exm: ExceptionMessage[_] => 
					throw exm.ex
				case ok: OkMessage[A] =>
					ok.ret
			}
	}
	def actorsSize = actors.size
	override def stopActors: Unit = {
		for(executor <- actors)
			executor.stop
		super.stopActors
	}
}

trait ManyActors extends AbstractManyActors[ExecutorActor]

trait AbstractTwoActors [E <: ExecutorActor] extends AbstractActorDsl[E] {
	val actor1 = newActor
	val actor2 = newActor
	def inActor1[A](f: => A) =
		actor1.execute(f)
	def inActor2[A](f: => A) = 
		actor2.execute(f)
	override def stopActors: Unit = {
		actor1.stop
		actor2.stop
		super.stopActors
	}
}

trait TwoActors extends AbstractTwoActors[ExecutorActor]