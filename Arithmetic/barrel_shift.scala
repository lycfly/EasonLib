package EasonLib.Arithmetic

import spinal.core._

import scala.language.postfixOps

class barrel_shift(WIDTH: Int = 32) extends Component {
  val WIDTH_LOG2 = log2Up(WIDTH)
  val io = new Bundle {
    val rotate_en = in Bool()
    val direction = in Bool() // 1: left shift, 0: right shift
    val signed = in Bool()    // when rotate_en , this bit will be ignored
    val shift_amount = in UInt(WIDTH_LOG2 bits)
    val data_in = in Bits(WIDTH bits)
    val data_out = out Bits(WIDTH bits)
  }
  noIoPrefix()
  val shifted_data = Bits(2*WIDTH bits)
  when(io.direction){
    shifted_data := (io.data_in.asUInt.resize(2*WIDTH) |<< io.shift_amount).asBits
    io.data_out := shifted_data(WIDTH-1 downto 0) | (shifted_data(2*WIDTH-1 downto WIDTH) & io.rotate_en.asSInt.resize(WIDTH).asBits)
  }.otherwise{
    when(io.signed){
      shifted_data := ((io.data_in ## B(0, WIDTH bits)).asSInt |>> io.shift_amount).asBits
    }.otherwise{
      shifted_data := ((io.data_in ## B(0, WIDTH bits)).asUInt |>> io.shift_amount).asBits
    }
    io.data_out := shifted_data(2*WIDTH-1 downto WIDTH) | (shifted_data(WIDTH-1 downto 0) & io.rotate_en.asSInt.resize(WIDTH).asBits)
  }

}

object barrel_shift_inst {
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
      .generate(new barrel_shift())
  }.printPruned()
}