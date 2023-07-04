package EasonLib.Arithmetic

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable
import scala.util.Random
import scala.language.postfixOps

class logic_unit(WIDTH: Int = 32) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val mode = in Bits(2 bits) // 0: or, 1: and, 2: xor
    val reverse_a = in  Bool()
    val reverse_b = in Bool()
    val din_a = in Bits(WIDTH bits)
    val din_b = in Bits(WIDTH bits)
    val data_out = out Bits(WIDTH bits)
  }
  noIoPrefix()
  val operand_a = io.reverse_a ? ~io.din_a | io.din_a
  val operand_b = io.reverse_b ? ~io.din_b | io.din_b
  io.data_out := 0
  when(io.din_vld){
    when(io.mode === 0) {
      io.data_out := operand_a | operand_b
    }.elsewhen(io.mode === 1){
      io.data_out := operand_a & operand_b
    }.elsewhen(io.mode === 2){
      io.data_out := operand_a ^ operand_b
    }
  }


}

object logic_unit_inst {
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
      .generate(new logic_unit())
  }.printPruned()
}