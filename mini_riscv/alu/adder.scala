package EasonLib.mini_riscv.alu

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class adder(SIZEIN:Int=32) extends Component {
  val io = new Bundle {
    val din_valid = in Bool()
    val is_sub = in Bool()
    val dinA = in Bits (SIZEIN bits)
    val dinB = in Bits (SIZEIN bits)
    val dout = out Bits (SIZEIN bits)
    val cout = out Bool()
    val neg_flag = out Bool()
    val ovflow_flag = out Bool()
    val zero_flag = out Bool()


  }
  noIoPrefix()
  val dinA_inner = io.din_valid ? io.dinA | B(0, SIZEIN bits)
  val dinB_inner = io.din_valid ? (io.is_sub ? ~io.dinB | io.dinB) | B(0, SIZEIN bits)
  val cin_inner = io.is_sub
  val add_result = (dinA_inner.asUInt +^ dinB_inner.asUInt + cin_inner.asUInt).asBits

  io.cout := add_result.msb
  io.dout := add_result(SIZEIN-1 downto 0)
  io.neg_flag := add_result(SIZEIN)
  io.ovflow_flag := (dinA_inner(SIZEIN - 1) & dinB_inner(SIZEIN - 1)) & (add_result(SIZEIN - 1) ^ dinB_inner(SIZEIN - 1))
  io.zero_flag := io.dout === 0
}

object adder_inst {
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
      .generate(new adder())
  }.printPruned()
}