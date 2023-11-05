package EasonLib.Common_ip

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class fsm() extends Area {
  object STATE extends SpinalEnum {
    val IDLE, WAIT_SQUARE1, SQUARE2, SQRT, NORM = newElement()

  }

}

