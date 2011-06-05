package net.fwbrasil.radon.util

import org.apache.commons.collections.map.{ ReferenceMap => ReferenceMapWrapped }
import org.apache.commons.collections.map.AbstractReferenceMap
import scala.collection.JavaConversions._
import scala.collection.generic._

class ReferenceWeakMap[A, B] extends JMapWrapper[A, B](new ReferenceMapWrapped(AbstractReferenceMap.WEAK, AbstractReferenceMap.WEAK).asInstanceOf[java.util.Map[A, B]])
			   with JMapWrapperLike[A, B, ReferenceWeakMap[A, B]] {
  override def empty = new ReferenceWeakMap[A, B]
}

object ReferenceWeakMap extends MutableMapFactory[ReferenceWeakMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ReferenceWeakMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: ReferenceWeakMap[A, B] = new ReferenceWeakMap[A, B]
}

class ReferenceWeakKeyMap[A, B] extends JMapWrapper[A, B](new ReferenceMapWrapped(AbstractReferenceMap.WEAK, AbstractReferenceMap.HARD).asInstanceOf[java.util.Map[A, B]])
			   with JMapWrapperLike[A, B, ReferenceWeakKeyMap[A, B]] {
  override def empty = new ReferenceWeakKeyMap[A, B]
}

object ReferenceWeakKeyMap extends MutableMapFactory[ReferenceWeakKeyMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ReferenceWeakKeyMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: ReferenceWeakKeyMap[A, B] = new ReferenceWeakKeyMap[A, B]
}

class ReferenceWeakValueMap[A, B] extends JMapWrapper[A, B](new ReferenceMapWrapped(AbstractReferenceMap.HARD, AbstractReferenceMap.WEAK).asInstanceOf[java.util.Map[A, B]])
			   with JMapWrapperLike[A, B, ReferenceWeakValueMap[A, B]] {
  override def empty = new ReferenceWeakValueMap[A, B]
}

object ReferenceWeakValueMap extends MutableMapFactory[ReferenceWeakValueMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ReferenceWeakValueMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: ReferenceWeakValueMap[A, B] = new ReferenceWeakValueMap[A, B]
}