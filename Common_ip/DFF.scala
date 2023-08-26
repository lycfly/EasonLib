package EasonLib.Common_ip

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

object dff {
  /** DFF with enable and clr */
  def apply[T<:Data](width: Int)(d: T, q: T, en: Bool, clr: Bool): Component = {
    val dff = new dff_ec(width)
    dff.io.d := d.asBits
    dff.io.ena := en
    dff.io.clr := clr
    q.assignFromBits(dff.io.q)
    dff
  }
  /** DFF with enable */
  def noclr[T<:Data](width: Int)(d: T, q: T, en: Bool): Component ={
    val dff = new dff_e(width)
    dff.io.d := d.asBits
    dff.io.ena := en
    q.assignFromBits(dff.io.q)
    dff
  }

  /** DFF with enable and clr, with specified init value*/
  def withInit[T<:Data](width: Int,initValue: Int)(d: T, q: T, en: Bool, clr: Bool): Component = {
    val dff = new dff_ec_with_init(width,initValue)
    dff.io.d := d.asBits
    dff.io.ena := en
    dff.io.clr := clr
    q.assignFromBits(dff.io.q)
    dff
  }
}
class dff_ec(Width:Int) extends Component {
  val io = new Bundle {
    val ena = in Bool()
    val clr = in Bool()
    val d = in Bits(Width bits)
    val q = out Bits(Width bits)
  }
  noIoPrefix()
  io.q.setAsReg()
  io.q.init(io.q.getZero)
  when(io.clr){
    io.q := io.q.getZero
  }.elsewhen(io.ena){
    io.q := io.d
  }
}
class dff_e(Width:Int) extends Component {
  val io = new Bundle {
    val ena = in Bool()
    val d = in Bits(Width bits)
    val q = out Bits(Width bits)
  }
  noIoPrefix()
  io.q.setAsReg()
  io.q.init(io.q.getZero)
  when(io.ena){
    io.q := io.d
  }
}

class dff_ec_with_init(Width:Int, InitValue: Int) extends Component {
  val io = new Bundle {
    val ena = in Bool()
    val clr = in Bool()
    val d = in Bits(Width bits)
    val q = out Bits(Width bits)
  }
  io.q.setAsReg()
  io.q.init(InitValue)
  when(io.clr) {
    io.q := io.q.getZero
  }.elsewhen(io.ena) {
    io.q := io.d
  }
}


class dff_test(Width:Int) extends Component {
  val test = false
  val io = new Bundle {
    val ena = in Bool()
    val clr = in Bool()
    val d = in SInt(Width bits)
    val q = out SInt(Width bits)
    val tt = test generate out(Bool())
  }
  dff.withInit[SInt](Width,0)( io.d, io.q, io.ena,io.clr)

  val testt = Bool().genIf(test)
  test generate{
    testt := True
    io.tt := testt
  }

//  if(test) {
//    io.b := testt & io.d(0)
//  }
//  else{
//    io.b := io.d(0)
//  }

}

object DFF_inst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode = Verilog,
      oneFilePerComponent = false,
      nameWhenByFile = false,
      inlineConditionalExpression = true,
      enumPrefixEnable = false,
      anonymSignalPrefix = "tmp",
      targetDirectory = "rtl")
      .addStandardMemBlackboxing(blackboxAll)
      .generate(new dff_test(8))
  }.printPruned()
}