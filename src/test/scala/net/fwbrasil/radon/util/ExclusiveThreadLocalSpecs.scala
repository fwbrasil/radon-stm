package net.fwbrasil.radon.util

import org.specs2.mutable._
import net.fwbrasil.radon.dsl.actor._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class ExclusiveThreadLocalSpecs extends Specification {
	
	class Dummy extends ExclusiveThreadLocalItem

	"An exclusive thread local" should {
		
		"set value only for the current thread" in {
			new ActorDsl with TwoActors with OneActorPerThread {
				val value1 = Option(new Dummy)
				val value2 = Option(new Dummy)
				val exclusiveThreadLocal = new ExclusiveThreadLocal[Dummy]
				inActor1 {
					exclusiveThreadLocal.set(value1)
					exclusiveThreadLocal.get must beEqualTo(value1)
				} 
				inActor2 {
					exclusiveThreadLocal.get must beEqualTo(None)
				}
				inActor2 {
					exclusiveThreadLocal.set(value2)
					exclusiveThreadLocal.get must beEqualTo(value2)
				}
				inActor1 {
					exclusiveThreadLocal.get must beEqualTo(value1)
				}
			} must not beNull
		}
		
		"accept a value that belongs only to the thread" in {
			new ActorDsl with TwoActors with OneActorPerThread {
				val value1 = Option(new Dummy)
				val value2 = Option(new Dummy)
				val exclusiveThreadLocal = new ExclusiveThreadLocal[Dummy]
				inActor1 {
					exclusiveThreadLocal.set(value1)
					exclusiveThreadLocal.get must beEqualTo(value1)
				}
				inActor2 {
					exclusiveThreadLocal.set(value2)
					exclusiveThreadLocal.get must beEqualTo(value2)
				}
			} must not beNull
		}
		
		"not accept a value that doesn't belong only to the thread" in {
			new ActorDsl with TwoActors with OneActorPerThread {
				val value1 = Option(new Dummy)
				val exclusiveThreadLocal = new ExclusiveThreadLocal[Dummy]
				inActor1 {
					exclusiveThreadLocal.set(value1)
					exclusiveThreadLocal.get must beEqualTo(value1)
				}
				inActor2 {
					exclusiveThreadLocal.set(value1) must throwA[IllegalStateException]
				} 
				inActor2 {
					exclusiveThreadLocal.get must beEqualTo(None)
				}
			} must not beNull
		}
		
		"stay consistent with many threads" in {
			new ActorDsl with ManyActors with OneActorPerThread {
				val exclusiveThreadLocal = new ExclusiveThreadLocal[Dummy]
				inParallelActors {
					val value = Option(new Dummy)
					exclusiveThreadLocal.set(value)
					exclusiveThreadLocal.get must beEqualTo(value)
				}
			} must not beNull
		}
	}
}