package net.fwbrasil.radon.ref

trait RefListener[T] {
    def notifyGet(ref: Ref[T]) = {

    }
    def notifyPut(ref: Ref[T], obj: Option[T]) = {

    }
}
