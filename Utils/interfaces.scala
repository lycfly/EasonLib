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

  def write(Address: UInt, Wdata: Bits): Unit = {
    ena := True
    wena := True
    addr := Address
    wdata := Wdata
  }

  def read(Address: UInt): Unit = {
    ena := True
    wena := False
    addr := Address
  }

  def idle(): Unit = {
    ena := False
    wena := False
    addr := 0
    wdata := 0

  }
}


case class memif_2(ADDRWD: Int, DATAWD: Int, WithSEL: Boolean = false) extends Bundle with IMasterSlave {
  val rd = Bool()             // high: valid
  val wr = Bool()            // high: wr , low: rd
  val addr = UInt(ADDRWD bits)
  val wdata = Bits(DATAWD bits)
  val wsel = Bits(DATAWD/8 bits).genIf(WithSEL)

  val rdata = Bits(DATAWD bits)

  override def asMaster(): Unit = {
    out(rd)
    out(wr)
    out(addr)
    out(wdata)
    if(WithSEL){
      out(wsel)
    }
    in(rdata)
  }

  def write(Address: UInt, Wdata: Bits): Unit = {
    rd := False
    wr := True
    addr := Address
    wdata := Wdata
  }

  def read(Address: UInt): Unit = {
    rd := True
    wr := False
    addr := Address
  }

  def idle(): Unit = {
    rd := False
    wr := False
    addr := 0
    wdata := 0

  }
}
