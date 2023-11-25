package EasonLib.mini_riscv.alu

import EasonLib.Arithmetic.divider.restoring_div_unsigned
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps

class div_wrapper(SIZEINA: Int = 32, SIZEINB: Int = 32) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val is_signed = in Bool()
    val dinA = in Bits (SIZEINA bits)
    val dinB = in Bits (SIZEINB bits)

    val busy = out Bool()
    val dout_vld = out Bool()
    val quot = out Bits (SIZEINA bits)
    val remainder = out Bits (SIZEINB bits)
  }
  noIoPrefix()
  io.busy.setAsReg().init(False)
  when(io.din_vld) {
    io.busy := True
  }.elsewhen(io.dout_vld) {
    io.busy := False
  }

  val divider = new restoring_div_unsigned(SIZEINA, SIZEINB)
  divider.io.din_vld := io.din_vld
  divider.io.dinA := io.dinA
  divider.io.dinB := io.dinB
  io.dout_vld := divider.io.dout_vld

  val quot_sign_out = Reg(Bool()) init (False)
  val rem_sign_out = Reg(Bool()) init (False)
  when(io.din_vld) {
    quot_sign_out := (io.dinA.msb ^ io.dinB.msb) & io.is_signed
    rem_sign_out := io.dinA.msb & io.is_signed
  }
  /**
   * The quotient of division by zero has all bits set, and the remainder of division by zero equals the dividend.
   * Signed division overflow occurs only when the most-negative integer is divided by -1.
   * The quotient of a signed division with overflow is equal to the dividend, and the remainder is zero.
   * Unsigned division overflow cannot occur.
   */
  val div_zero_flag = io.dinB === 0

  io.remainder := Mux(rem_sign_out & ~div_zero_flag, (~divider.io.remainder.asSInt + 1).asBits, divider.io.remainder)
  when(div_zero_flag){
    io.quot.setAll()
  }.otherwise{
    io.quot := Mux(quot_sign_out, ((~divider.io.quot).asUInt + 1).asBits, divider.io.quot)
  }
}

object div_wrapper_inst {
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
      .generate(new div_wrapper())
  }.printPruned()
}