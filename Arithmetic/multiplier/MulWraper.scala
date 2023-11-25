package EasonLib.Arithmetic.multiplier

import spinal.core._
import spinal.sim._
import spinal.lib._
case class MUlConfig(
                      DATAWDA: Int ,
                      DATAWDB: Int ,
                      MulVender: MulBaseVender,
                      use_busy_flag: Boolean = false,
                    )

case class MulInterface(mc: MUlConfig) extends Bundle with IMasterSlave {
  val din_vld =  Bool()
  val dinA =  Bits (mc.DATAWDA bits)
  val dinB =  Bits (mc.DATAWDB bits)
  val dout_vld =  Bool()
  val dout =  Bits (mc.DATAWDA + mc.DATAWDB bits)
  val busy = mc.use_busy_flag generate Bool()

  override def asMaster(): Unit = {
    out(dinA)
    out(dinB)
    out(din_vld)
    in(dout)
    in(dout_vld)
    if(mc.use_busy_flag){
      in(busy)
    }
  }
}
case class MulWraper(mc: MUlConfig) extends Component{
  val io = new Bundle{
    val mulif = slave(MulInterface(mc))
  }
  noIoPrefix()
  val mul_unit = mc.MulVender.build(this, mc)
}
trait MulBaseVender {
  def connect_busy_flag(wrap: MulWraper, mc :MUlConfig): Unit = {
    if (mc.use_busy_flag) {
      val busy_reg = RegInit(False)
      when(wrap.io.mulif.din_vld) {
        busy_reg := True
      }.elsewhen(wrap.io.mulif.dout_vld) {
        busy_reg := False
      }
      wrap.io.mulif.busy := busy_reg
    }
  }
  def build(wrap: MulWraper, mc :MUlConfig): Component = {
    val mul = new SignMultiplier(mc.DATAWDA,mc.DATAWDB,withInReg = false, withOutReg = false)
    mul.io.dinA := wrap.io.mulif.dinA.asSInt
    mul.io.dinB := wrap.io.mulif.dinB.asSInt
    mul.io.din_vld := wrap.io.mulif.din_vld
    wrap.io.mulif.dout := mul.io.dout.asBits
    wrap.io.mulif.dout_vld := mul.io.dout_vld
    connect_busy_flag(wrap, mc)
    mul
  }
}

case object CombSignedMultiplier extends MulBaseVender
case object Booth4SignedMultiplier extends MulBaseVender {
  override def build(wrap: MulWraper, mc :MUlConfig): Component = {
    val mul = new booth4(mc.DATAWDA, mc.DATAWDB)
    mul.io.dinA := wrap.io.mulif.dinA.asSInt
    mul.io.dinB := wrap.io.mulif.dinB.asSInt
    mul.io.din_vld := wrap.io.mulif.din_vld
    wrap.io.mulif.dout := mul.io.dout.asBits
    wrap.io.mulif.dout_vld := mul.io.dout_vld
    connect_busy_flag(wrap, mc)
    mul
  }
}


object Mul_testinst {
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
      .generate({
        val mc = MUlConfig(8,8, CombSignedMultiplier,use_busy_flag = true)
        new MulWraper(mc)
      })
  }.printPruned()
}