package EasonLib.Arithmetic

import spinal.core._

import scala.language.postfixOps

class Round2Even(DataWidth: Int,RoundBits: Int) extends Component {
  val io = new Bundle {
    val din = in SInt (DataWidth bits)
    val dout = out SInt(DataWidth-RoundBits+1 bits)

  }
  noIoPrefix()
  io.dout := io.din.roundToEven(RoundBits,false)

}
object Round2EvenInst {
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
  ).generate(new Round2Even(8, 4))

    //    //SpinalVerilog(new butterfly(SizeXIn = 16,SizeWIn = 16,SizeOut = 16,para = 5))
    //    val compiled = SimConfig.withWave.allOptimisation.compile(
    //      rtl = new RoundandSat(SizeInList = Array(1,20,15), SizeOutList = Array(1,16,15)))

  }
}