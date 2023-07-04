package EasonLib.mini_riscv.alu

import EasonLib.Arithmetic.{SignMultiplier, booth4}
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps

import scala.collection.mutable

class mul_wrapper(WIDTH: Int = 32, MULTI_CYCLE_MUL: Boolean = true) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val is_A_signed = in Bool()
    val is_B_signed = in Bool()
    val dinA = in Bits (WIDTH bits)
    val dinB = in Bits (WIDTH bits)

    val busy = out Bool()
    val dout_vld = out Bool()
    val dout = out Bits (WIDTH*2 bits)
  }
  noIoPrefix()
  val ext_bits_A = io.is_A_signed & io.dinA.msb
  val ext_bits_B = io.is_B_signed & io.dinA.msb

  val dinA_inner = io.din_vld ? (ext_bits_A ## ext_bits_A ## io.dinA) | B(0, WIDTH+2 bits)
  val dinB_inner = io.din_vld ? (ext_bits_B ## io.dinB) | B(0, WIDTH+1 bits)

  io.busy.setAsReg().init(False)
  when(io.din_vld){
    io.busy := True
  }.elsewhen(io.dout_vld){
    io.busy := False
  }


  if(MULTI_CYCLE_MUL){
    val mul = new booth4(WIDTH+2,WIDTH+1)
    mul.io.din_vld := io.din_vld
    mul.io.dinA := dinA_inner.asSInt
    mul.io.dinB := dinB_inner.asSInt
    io.dout_vld := mul.io.dout_vld
    io.dout := mul.io.dout(WIDTH*2-1 downto 0).asBits
    mul.io.cal_finish.allowPruning()
  }
  else{
    val mul = new SignMultiplier(WIDTH+2,WIDTH+1, withOutReg = false)
    mul.io.din_vld := io.din_vld
    mul.io.dinA := dinA_inner.asSInt
    mul.io.dinB := dinB_inner.asSInt
    io.dout_vld := mul.io.dout_vld
    io.dout := mul.io.dout(WIDTH*2-1 downto 0).asBits
  }

}

object mul_wrapper_inst {
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
      .generate(new mul_wrapper())
  }.printPruned()
}