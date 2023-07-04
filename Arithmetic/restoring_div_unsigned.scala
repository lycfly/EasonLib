package EasonLib.Arithmetic

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class restoring_div_unsigned(SIZEINA: Int, SIZEINB: Int) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val dinA = in Bits(SIZEINA bits)
    val dinB = in Bits(SIZEINB bits)

    val dout_vld = out Bool()
    val quot = out Bits(SIZEINA bits)
    val remainder = out Bits(SIZEINB bits)

  }
  noIoPrefix()

  val dinA_abs = UInt(SIZEINA bits)
  val dinB_abs = UInt(SIZEINB bits)
  dinA_abs := io.dinA.asUInt
  dinB_abs := io.dinB.asUInt

  val quotient = Reg(Bits(SIZEINA bits)) init (0)

  val divisior = Reg(Bits(SIZEINB + 1 bits)) init (0)

  when(io.din_vld) {
    divisior := dinB_abs.asBits.resized
  }

  val PR_DW = SIZEINA + SIZEINB
  val p_remainder = Reg(Bits(PR_DW + 1 bits)) init (0)
  val p_r_shift = p_remainder |<< 1
  val p_r_calpart = Bits(SIZEINB + 1 bits)
  val p_r_minus_d = Bits(SIZEINB + 1 bits)
  val p_r_part_next = Bits(SIZEINB + 1 bits)

  p_r_calpart := p_r_shift(PR_DW downto SIZEINA)
  p_r_minus_d := (p_r_calpart.asSInt - divisior.asSInt).asBits

  p_r_part_next := Mux(p_r_minus_d.msb, p_r_calpart, p_r_minus_d)

  val CNT_DW = log2Up(SIZEINA)
  val control_cnt = Reg(UInt(CNT_DW bits)) init (0)
  val doing = Reg(Bool()) init (False)
  val finish_pulse = control_cnt === SIZEINA - 1
  when(io.din_vld) {
    doing.set()
  }.elsewhen(finish_pulse) {
    doing.clear()
  }
  when(io.din_vld) {
    control_cnt := 0
  }.elsewhen(doing) {
    control_cnt := control_cnt + 1
  }.elsewhen(finish_pulse) {
    control_cnt := 0
  }

  when(io.din_vld) {
    p_remainder := B(0, SIZEINB + 1 bits) ## dinA_abs
  }.elsewhen(doing) {
    p_remainder := p_r_part_next ## p_r_shift(SIZEINA - 1 downto 0)
  }
  val quot_reversed = Bits(SIZEINA bits)
  for (i <- 0 to SIZEINA - 1) {
    when(io.din_vld) {
      quotient(i) := False
    }.elsewhen(doing) {
      when(control_cnt === i) {
        quotient(i) := ~p_r_minus_d.msb
      }
    }
  }
  var i = 0
  quot_reversed := quotient.reversed

  io.remainder := p_remainder(PR_DW - 1 downto SIZEINA)
  io.quot := quot_reversed
  io.dout_vld := RegNext(finish_pulse) init (False)

}

object restoring_div_unsigned_inst {
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
      .generate(new restoring_div_unsigned(SIZEINA = 20, SIZEINB = 10))
  }.printPruned()
}

