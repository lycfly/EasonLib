package EasonLib.Arithmetic

import spinal.core._

import scala.language.postfixOps

class Round2EvenSW(DINW: Int, DOUTW: Int) extends Component {
  val io = new Bundle {
    val din = in SInt (DINW bits)
    val dout = out SInt(DOUTW bits)
    val right_shift = in UInt(log2Up(DINW) bits)
  }
  noIoPrefix()
  val signbit = io.din.msb
  val din_abs = Mux(signbit, ~io.din, io.din).asUInt + signbit.asUInt

  val decimal_part = din_abs.asBits & ((U(1,DINW bits) |<< (io.right_shift + 1)) - 1).asBits
  val decide_value = Bits(DINW bits)
  decide_value := (U(1,DINW bits) |<< (io.right_shift - 1)).asBits
  val cond = decimal_part === decimal_part
  val result = UInt(DINW bits)
  result := (din_abs |>> io.right_shift) + (din_abs(io.right_shift - 1) & cond).asUInt

  val result_clip = result(DOUTW-1 downto 0)
  io.dout :=  (Mux(signbit, ~result_clip, result_clip) + signbit.asUInt).asSInt

}
object Round2EvenSWInst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode = Verilog,
      oneFilePerComponent = true,
      nameWhenByFile = false,
      inlineConditionalExpression = true,
      enumPrefixEnable = false,
      anonymSignalPrefix = "tmp",
      targetDirectory = "rtl"
    ).generate(new Round2EvenSW(8, 4))

    //    //SpinalVerilog(new butterfly(SizeXIn = 16,SizeWIn = 16,SizeOut = 16,para = 5))
    //    val compiled = SimConfig.withWave.allOptimisation.compile(
    //      rtl = new RoundandSat(SizeInList = Array(1,20,15), SizeOutList = Array(1,16,15)))

  }
}