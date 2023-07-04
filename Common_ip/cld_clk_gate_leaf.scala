package EasonLib.Common_ip

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class cld_clk_gate_leaf() extends BlackBox {
  val io = new Bundle {
    val clk_i = in Bool()
    val test_en_i = in Bool()
    val en_i = in Bool()
    val gclk_o = out Bool()

  }
  noIoPrefix()
  // Map the current clock domain to the io.clk pin
  mapClockDomain(clock = io.clk_i)
  addRTLPath("verilog_ips/common_ip/cld_clk_gate_leaf.v")
}

object cld_clk_gate_leaf_inst {
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
      .generate(new cld_clk_gate_leaf())
  }.printPruned()
}