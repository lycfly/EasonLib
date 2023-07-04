package EasonLib.Utils

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

case class memif(ADDRWD: Int, DATAWD: Int, WithSEL: Boolean = false) extends Bundle with IMasterSlave {
  val ena = Bool()             // high: valid
  val wena = Bool()            // high: wr , low: rd
  val addr = UInt(ADDRWD bits)
  val wdata = Bits(DATAWD bits)
  val wsel = Bits(DATAWD/8 bits).genIf(WithSEL)

  val rdata = Bits(DATAWD bits)

  override def asMaster(): Unit = {
    out(ena)
    out(wena)
    out(addr)
    out(wdata)
    if(WithSEL){
      out(wsel)
    }
    in(rdata)
  }
}

