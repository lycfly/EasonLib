package EasonLib.Arithmetic

import spinal.core._
import spinal.core.sim.{SimConfig, sleep}

import scala.language.postfixOps

class RoundandSat(SizeInList: Array[Int], SizeOutList: Array[Int]) extends Component {
  val io = new Bundle {
    val din = in Bits(SizeInList(1) bits)
    val dout = out Bits(SizeOutList(1) bits)

  }
  noIoPrefix()
  val RoundBit = SizeInList(2)- SizeOutList(2)
  val SatBit = SizeInList(1) - SizeOutList(1) - RoundBit  // support sat bit < 0 (expend with sign bit)
  //val dataRound = if(SizeOutList(0)==1) SInt() else UInt()
  val dataOut = if(SizeOutList(0)==1) SInt(SizeOutList(1) bits) else UInt(SizeOutList(1) bits)
  dataOut.addAttribute("keep")

  if(SizeInList(0)==1){
    dataOut := io.din.asSInt.roundToInf(RoundBit).sat(SatBit)
  }else{
    dataOut := io.din.asUInt.roundToInf(RoundBit).sat(SatBit)
  }
  io.dout := dataOut.asBits
}
object RoundandSatInst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode=Verilog).generate(new RoundandSat(SizeInList = Array(1,20,15), SizeOutList = Array(1,16,12)))

    //SpinalVerilog(new butterfly(SizeXIn = 16,SizeWIn = 16,SizeOut = 16,para = 5))
    val compiled = SimConfig.withWave.allOptimisation.compile(
      rtl = new RoundandSat(SizeInList = Array(1,20,15), SizeOutList = Array(1,16,15)))

  }
}