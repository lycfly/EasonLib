package EasonLib.Arithmetic.divider



import spinal.core._
import spinal.sim._
import spinal.lib._
case class DivConfig(
                      DATAWDA: Int ,
                      DATAWDB: Int ,
                      DivVender: DivBaseVender,
                      use_busy_flag: Boolean = false,
                    )

case class DivInterface(dc: DivConfig) extends Bundle with IMasterSlave {
  val din_vld =  Bool()
  val dinA =  Bits (dc.DATAWDA bits)
  val dinB =  Bits (dc.DATAWDB bits)
  val dout_vld =  Bool()
  val quot =  Bits (dc.DATAWDA + 1 bits)
  val remainder = Bits(dc.DATAWDB bits)
  val busy = dc.use_busy_flag generate Bool()

  override def asMaster(): Unit = {
    out(dinA)
    out(dinB)
    out(din_vld)
    in(quot)
    in(remainder)
    in(dout_vld)
    if(dc.use_busy_flag){
      in(busy)
    }
  }
}
case class DivWraper(dc: DivConfig) extends Component{
  val io = new Bundle{
    val divif = slave(DivInterface(dc))
  }
  noIoPrefix()
  val mul_unit = dc.DivVender.build(this, dc)
}
trait DivBaseVender {
  def connect_busy_flag(wrap: DivWraper, dc :DivConfig): Unit = {
    if (dc.use_busy_flag) {
      val busy_reg = RegInit(False)
      when(wrap.io.divif.din_vld) {
        busy_reg := True
      }.elsewhen(wrap.io.divif.dout_vld) {
        busy_reg := False
      }
      wrap.io.divif.busy := busy_reg
    }
  }
  def build(wrap: DivWraper, dc :DivConfig): Component = {
    val div = new restoring_div(dc.DATAWDA,dc.DATAWDB)
    div.io.dinA := wrap.io.divif.dinA.asSInt
    div.io.dinB := wrap.io.divif.dinB.asSInt
    div.io.din_vld := wrap.io.divif.din_vld
    wrap.io.divif.quot := div.io.quot.asBits
    wrap.io.divif.remainder := div.io.remainder.asBits
    wrap.io.divif.dout_vld := div.io.dout_vld
    connect_busy_flag(wrap, dc)
    div
  }
}

case object RestoringSignedDivider extends DivBaseVender


object Div_testinst {
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
        val dc = DivConfig(8,8, RestoringSignedDivider,use_busy_flag = true)
        new DivWraper(dc)
      })
  }.printPruned()
}