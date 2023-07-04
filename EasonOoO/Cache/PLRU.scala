package EasonLib.EasonOoO.Cache

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.math.pow
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps

class PLRU(ASSOCIATIVITY: Int = 8,
           ENTRIES: Int = 256) extends Component {
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

  def age_flag_gen(ASSO: Int, lru_update_vld: Bool, stage: Int): Bits = {
    var age_bit_stage = Reg(Bits(ENTRIES bits)) init(0)
    var reference_bit = io.lru_update.payload(stage)
    if (ASSO == 2) {
      var least_recently_used_way_inner = Bits(1 bits)
      least_recently_used_way_inner := age_bit_stage(io.line_index).asBits
      when(lru_update_vld) {
        age_bit_stage(io.line_index) := (~reference_bit)
      }
      least_recently_used_way_inner
    }
    else {
      var half_sel = Bool()
      var lru_way0 = Bits()
      var lru_way1 = Bits()
      var least_recently_used_way = Bits()
      var lru_update_half0 = Bool()
      var lru_update_half1 = Bool()
      lru_update_half0 := ~reference_bit & lru_update_vld
      lru_update_half1 := reference_bit & lru_update_vld
      half_sel := age_bit_stage(io.line_index)
      lru_way0 := age_flag_gen(ASSO / 2, lru_update_half0, stage - 1)
      lru_way1 := age_flag_gen(ASSO / 2, lru_update_half1, stage - 1)

      when(lru_update_vld) {
        age_bit_stage(io.line_index) := (~reference_bit)
      }
      least_recently_used_way := Mux(half_sel, False ## lru_way0, True ## lru_way1)
      least_recently_used_way
    }
  }


  io.lru_way := age_flag_gen(ASSOCIATIVITY, io.lru_update.valid, ASSOC_LOG2-1).asUInt

}

object PLRU_inst {
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
      .generate(new PLRU())
  }.printPruned()
}