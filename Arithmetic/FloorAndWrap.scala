package EasonLib.Arithmetic

import spinal.core._

import scala.language.postfixOps

class FloorAndWrap(SizeInList: Array[Int], SizeOutList: Array[Int]) extends Component {
  val io = new Bundle {
    val din = in Bits(SizeInList(1) bits)
    val dout = out Bits(SizeOutList(1) bits)

  }
  noIoPrefix()
  val RoundBit = SizeInList(2)- SizeOutList(2)
  val SatBit = SizeInList(1) - SizeOutList(1) - RoundBit
  val dataOut = if(SizeOutList(0)==1) SInt(SizeOutList(1) bits) else UInt(SizeOutList(1) bits)

  if(SizeInList(0)==1){
    dataOut := io.din.asSInt.floor(RoundBit).trim(SatBit)
  }else{
    dataOut := io.din.asUInt.floor(RoundBit).trim(SatBit)
  }
  io.dout := dataOut.asBits
}
object FloorAndWrapInst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode=Verilog).generate(new FloorAndWrap(SizeInList = Array(1,20,15), SizeOutList = Array(1,16,12)))

    //    //SpinalVerilog(new butterfly(SizeXIn = 16,SizeWIn = 16,SizeOut = 16,para = 5))
    //    val compiled = SimConfig.withWave.allOptimisation.compile(
    //      rtl = new RoundandSat(SizeInList = Array(1,20,15), SizeOutList = Array(1,16,15)))

  }
}