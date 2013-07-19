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

}