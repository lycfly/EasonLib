package EasonLib.Common_ip

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps

object lzc{
  def apply(Name: String = "", WIDTH: Int, mode: Bool, lead: Bool, trail: Bool, data_in: Bits): UInt = {
    val lzc_unit = new lzc(WIDTH)
    if(Name.isEmpty){}
    else
      lzc_unit.setName(Name)
    lzc_unit.io.mode := mode
    lzc_unit.io.lead := lead
    lzc_unit.io.trail := trail
    lzc_unit.io.data_in := data_in
    lzc_unit.io.cnt_out
  }
}

/**
Count leading/trailing Zero/One of a given Bits val
  for example:
    data_in = 4'b1100  mode = trailing one
    cnt_out = 2
  ----
  if input is not valid , then cnt_out = Length of input bits.
  for example:
    data_in = 4'b0000  mode = trailing one
    cnt_out = 4
  */

class lzc(WIDTH: Int = 32) extends Component {
  val WIDTH_LOG2 = log2Up(WIDTH)
  val io = new Bundle {
    val mode = in Bool() // 0: count 0, 1: count 1.
    //    val lead_or_trail = Bool() // 0: lead , 1: trail
    val lead = in Bool()
    val trail = in Bool()
    val data_in = in Bits (WIDTH bits)
    val cnt_out = out UInt (log2Up(WIDTH + 1) bits)
  }
  noIoPrefix()
  val data_s = Bits(WIDTH bits)
  val datain_reverse = io.data_in.reversed

  data_s := 0
  when(io.mode) {
    when(io.lead) { // leading 1
      data_s := ~datain_reverse
    }.elsewhen(io.trail) { // trailing 1
      data_s := ~io.data_in
    }
  }.otherwise {
    when(io.lead) { // leading 0
      data_s := datain_reverse
    }.elsewhen(io.trail) { // trailing 0
      data_s := io.data_in
    }
  }


  /**
   * FIXME: 需要支持非2的幂次，可以省去很多节点。
   */
  val stage_node = Vec(Bits(), WIDTH_LOG2)
  val stage_index = ArrayBuffer[ArrayBuffer[UInt]]()
  for (i <- 0 to WIDTH_LOG2) {
    val STAGE_ELENUM = scala.math.pow(2, WIDTH_LOG2 - i).toInt
    stage_index += ArrayBuffer.fill(STAGE_ELENUM)(UInt(WIDTH_LOG2 bits))
  }
  stage_node(0) := data_s
  for (i <- 0 until scala.math.pow(2, WIDTH_LOG2).toInt) {
    stage_index(0)(i) := U(i, WIDTH_LOG2 bits)
  }
  assert(WIDTH_LOG2 > 0, "Input data width should > 0")
  if (WIDTH == 1) {
    io.cnt_out := (~data_s).asUInt
  }
  else {
    for (i <- 1 to WIDTH_LOG2) {
      val STAGE_ELENUM = scala.math.pow(2, WIDTH_LOG2 - i).toInt
      if (i != WIDTH_LOG2) {
        stage_node(i).setWidth(STAGE_ELENUM)
        for (j <- 0 until STAGE_ELENUM) {
          stage_node(i)(j) := stage_node(i - 1)(2 * j) | stage_node(i - 1)(2 * j + 1)
        }
      }
      for (j <- 0 until STAGE_ELENUM) {
        stage_index(i)(j).setName("stage_index" + i.toString + "_" + j.toString) := Mux(stage_node(i - 1)(2 * j), stage_index(i - 1)(2 * j), stage_index(i - 1)(2 * j + 1))

      }

    }
  }

  val empty = (io.lead | io.trail) ? ~data_s.orR | False

  io.cnt_out := empty ? U(WIDTH) | stage_index(WIDTH_LOG2)(0)
}

object lzc_inst {
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
      .generate(new lzc())
  }.printPruned()
}