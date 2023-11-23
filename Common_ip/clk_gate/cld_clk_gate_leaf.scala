package EasonLib.Common_ip.clk_gate

import spinal.core._

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
  addRTLPath("src/main/scala/EasonLib/verilog_ips/common_ip/cld_clk_gate_leaf.v")
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
      targetDirectory = "rtl_gen")
      .addStandardMemBlackboxing(blackboxAll)
      .generate(new cld_clk_gate_leaf())
  }.printPruned()
}