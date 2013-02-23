package net.fwbrasil.radon.util

import ThreadUtil._

object Debug {

    var isDebuggin = false
    val debugParkingLot = new ParkingLot

    def setDebug = isDebuggin = true
    def setNormal = {
        isDebuggin = false
        debugParkingLot.unparkAll
    }

    def parkIfDebug =
        if (isDebuggin)
            debugParkingLot.park
}