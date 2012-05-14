//package net.fwbrasil.radon.util
//
//import org.specs2.mutable._
//import org.junit.runner._
//import org.specs2.runner._
//
//@RunWith(classOf[JUnitRunner])
//class ReferenceWeakMapSpecs extends Specification {
//
//	"ReferenceWeakMap" should {
//		"remove tuple" in {
//			"if value and key are garbage collected" in {
//				val map = new ReferenceWeakMap[Object, Object]()
//				map += (new Object -> new Object)
//				map.size must beEqualTo(1)
//				GCUtil.runGC
//				map.size must beEqualTo(0)
//			}
//			"if value is garbage collected" in {
//				val map = new ReferenceWeakMap[Object, Object]()
//				val key = new Object
//				map += (key -> new Object)
//				map.size must beEqualTo(1)
//				GCUtil.runGC
//				map.size must beEqualTo(0)
//			}
//			"if key is garbage collected" in {
//				val map = new ReferenceWeakMap[Object, Object]()
//				val value = new Object
//				map += (new Object -> value)
//				map.size must beEqualTo(1)
//				GCUtil.runGC
//				map.size must beEqualTo(0)
//			}
//		}
//		"preserve tuple if value and key has strong references" in {
//			val map = new ReferenceWeakMap[Object, Object]()
//			val key = new Object
//			val value = new Object
//			map += (key -> value)
//			map.size must beEqualTo(1)
//			GCUtil.runGC
//			map.size must beEqualTo(1)
//		}
//	}
//
//}