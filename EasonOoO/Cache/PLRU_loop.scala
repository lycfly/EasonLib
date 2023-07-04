package EasonLib.EasonOoO.Cache

import scala.math.pow
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps

class PLRU_loop(ASSOCIATIVITY: Int = 4,
           ENTRIES: Int = 32) extends Component {
  val ASSOC_LOG2 = log2Up(ASSOCIATIVITY)
  val ENTRIES_LOG2 = log2Up(ENTRIES)

  val io = new Bundle {
    //Read Port
    val line_index = in UInt (ENTRIES_LOG2 bits)
    val lru_way = out UInt (ASSOC_LOG2 bits)
    //Update Port
    val lru_update = slave(Flow(UInt(ASSOC_LOG2 bits)))
  }
  noIoPrefix()

    var age_bit_stage = Vec(Vec(Reg(Bits()) init(0), ASSOC_LOG2), ENTRIES)
//     var age_bit_stage = ArrayBuffer[Any]()

    for(i <- 0 until ENTRIES){
      for (j <- 0 until ASSOC_LOG2) {
        var AGEBITSWIDTH = pow(2, j).toInt
        age_bit_stage(i)(j).setWidth(AGEBITSWIDTH)
      }
    }

//  val age_bit_line = age_bit_stage(io.line_index)
//    for (j <- 0 until ASSOC_LOG2) {
//      var ref_bit = io.lru_update.payload(ASSOC_LOG2-1-j)
//      var age_bit_group = age_bit_line(j)
//      var update_bits_index = io.lru_update.payload(j downto 0)
//      when(io.lru_update.valid){
//        age_bit_group(update_bits_index) := ~ref_bit
//      }
//
//    }

    io.lru_way := 0

//  io.lru_way(ASSOC_LOG2-1) := age_bit_line(0).asBool
//  if(ASSOC_LOG2>1){
//    for (j <- 0 until ASSOC_LOG2 - 1) {
//      var next_index = UInt()
//      next_index := io.lru_way(ASSOC_LOG2-1 downto ASSOC_LOG2-1-j)
//      io.lru_way(ASSOC_LOG2 - 2 - j) := age_bit_line(j+1)(next_index)
//    }
//  }


//  def find_lsu(Stage: Int, ref_bit: Bool, cand_2bits: Bits): Bits = {
//    if(Stage == 0){
//      var out_bits = Mux(ref_bit, cand_2bits(1), cand_2bits(0))
//      out_bits.asBits
//    }
//    else{
//      find_lsu(Stage-1, io.lru_way(Stage), cand_2bits: Bits)
//    }
//
//  }


}

object PLRU_loop_inst {
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
      .generate(new PLRU_loop())
  }.printPruned()
}