package EasonLib.mini_riscv.decode

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class pre_decode(WIDTH: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val instr_in = in Bits(WIDTH bits)
    val instr_type = in Bool() //0: 16bits, 1: 32 bits
    val rv_predec = out(rv_predec_if(REGNUM))
  }
  noIoPrefix()
  val rvc_predecoder = new rvc_pre_dec(WIDTH, REGNUM)
  val rv_predecoder = new rv_pre_dec(WIDTH, REGNUM)

  rvc_predecoder.io.instr_in := io.instr_in
  rv_predecoder.io.instr_in := io.instr_in

  io.rv_predec := io.instr_type ? rv_predecoder.io.rv_predec| rvc_predecoder.io.rvc_predec


}

object pre_decode_inst {
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
      .generate(new pre_decode())
  }.printPruned()
}