package net.fwbrasil.radon.util

import net.fwbrasil.radon.dsl.actor._
import org.specs2.mutable._
import org.junit.runner._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class LockableSpecs extends Specification {

    "Lockable" should {
        "accept multiple read locks" in {
            new ActorDsl with ManyActors with OneActorPerThread {
                val lockable = new Lockable {}
                inActors {
                    lockable.tryReadLock
                }
                inMainActor {
                    lockable.readLockCount must beEqualTo(actorsSize)
                }
                inActors {
                    lockable.readUnlock
                }
                inMainActor {
                    lockable.readLockCount must beEqualTo(0)
                }
            } must not beNull
        }

        "accept only one write lock" in {
            new ActorDsl with TwoActors with OneActorPerThread {
                val lockable = new Lockable {}

                inActor1 {
                    lockable.tryWriteLock must beTrue
                }
                inMainActor {
                    lockable.isWriteLocked must beTrue
                }
                inActor2 {
                    lockable.tryWriteLock must beFalse
                }
                inActor1 {
                    lockable.writeUnlock
                }
                inActor2 {
                    lockable.tryWriteLock must beTrue
                }
                inMainActor {
                    lockable.isWriteLocked must beTrue
                }
                inActor2 {
                    lockable.writeUnlock
                }
                inMainActor {
                    lockable.isWriteLocked must beFalse
                }
            } must not beNull
        }

        "not accept a write lock if has read lock" in {
            new ActorDsl with TwoActors with OneActorPerThread {
                val lockable = new Lockable {}
                inActor1 {
                    lockable.tryReadLock must beTrue
                }
                inMainActor {
                    lockable.isWriteLocked must beFalse
                    lockable.readLockCount must beEqualTo(1)
                }
                inActor2 {
                    lockable.tryWriteLock must beFalse
                }
                inActor1 {
                    lockable.readUnlock
                }
                inMainActor {
                    lockable.isWriteLocked must beFalse
                    lockable.readLockCount must beEqualTo(0)
                }
            } must not beNull
        }

        "not accept a read lock if has write lock" in {
            new ActorDsl with TwoActors with OneActorPerThread {
                val lockable = new Lockable {}
                inActor1 {
                    lockable.tryWriteLock must beTrue
                }
                inMainActor {
                    lockable.isWriteLocked must beTrue
                    lockable.readLockCount must beEqualTo(0)
                }
                inActor2 {
                    lockable.tryReadLock must beFalse
                }
                inActor1 {
                    lockable.writeUnlock
                }
                inMainActor {
                    lockable.isWriteLocked must beFalse
                    lockable.readLockCount must beEqualTo(0)
                }
            } must not beNull
        }

    }
}