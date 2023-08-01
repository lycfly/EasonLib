package EasonLib.Common_ip

import spinal.core
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps
object popcount{
  def apply(Name: String = "", WIDTH: Int, din_vld: Bool, data_in: Bits): UInt = {
    val popc = new popcount(WIDTH)
    if (Name.isEmpty) {
      popc.setName(data_in.getName()+"_popc")
    }
    else
      popc.setName(Name)
    popc.io.din_vld := din_vld
    popc.io.data_in := data_in
    popc.io.cnt_out
  }
}
class popcount(WIDTH: Int = 32) extends Component {
  val WIDTH_LOG2 = log2Up(WIDTH)
  val io = new Bundle {
    val din_vld = in Bool()
    val data_in = in Bits(WIDTH bits)
    val cnt_out = out UInt(WIDTH_LOG2+1 bits)
  }
  noIoPrefix()
  val popsum_vec = ArrayBuffer[Any]()
  val popsum_init =  for(i <- 0 until WIDTH) yield io.data_in(i).asUInt
  popsum_vec += popsum_init
  for(i <- 1 to WIDTH_LOG2){
    var layer_ele_num = scala.math.pow(2,WIDTH_LOG2-i).toInt
    var popsum = for(j <- 0 until layer_ele_num) yield UInt()
    for(j <- 0 until layer_ele_num){
      popsum(j).setName(s"popsum_stage${i}_ele${j}") := popsum_vec(i-1).asInstanceOf[Iterable[Data]](2*j).asInstanceOf[UInt] +^ popsum_vec(i-1).asInstanceOf[Iterable[Data]](2*j+1).asInstanceOf[UInt]
    }
    popsum_vec += popsum
  }

  io.cnt_out := io.din_vld ? popsum_vec(WIDTH_LOG2).asInstanceOf[Iterable[Data]](0).asInstanceOf[UInt] | U(0)

}

object popcount_inst {
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
      .generate(new popcount())
  }.printPruned()
}