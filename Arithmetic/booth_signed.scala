package EasonLib.Arithmetic

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class booth_signed(SIZEINA: Int=8, SIZEINB:Int=8, RADIX: Int = 4) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val dinA = in SInt (SIZEINA bits)
    val dinB = in SInt (SIZEINB bits)

    val dout_vld = out Bool()
    val cal_finish = out Bool()
    val dout = out SInt (SIZEINA + SIZEINB bits)

  }
  noIoPrefix()

  // val dinAReg = Reg(SInt (SIZEINA bits)) init(0)
  // val dinBReg = Reg(SInt (SIZEINA bits)) init(0)
  val RADIX_LOG2 = log2Up(RADIX)
  assert(SIZEINA % (log2Up(RADIX)) == 0, "booth input A must be log2(RADIX)*N bitwidth!")
  val MAX_NUM = SIZEINA / RADIX_LOG2 - 1
  val MAX_DW = log2Up(SIZEINA / RADIX_LOG2)
  val SHIFT_SIZE = SIZEINA + SIZEINB + 1 + 2
  val Breg = Reg(SInt(SIZEINB bits)) init (0)
  val shiftReg = Reg(Bits(SHIFT_SIZE bits)) init (0)
  val flag_bits = Bits(3 bits)
  val NegativeB = SInt(SIZEINB + 2 bits)
  val Negative2B = SInt(SIZEINB + 2 bits)
  val PositiveB = SInt(SIZEINB + 2 bits)
  val Positive2B = SInt(SIZEINB + 2 bits)
  val AddB = SInt(SIZEINB + 2 bits)
  val Add2B = SInt(SIZEINB + 2 bits)
  val MinusB = SInt(SIZEINB + 2 bits)
  val Minus2B = SInt(SIZEINB + 2 bits)

  val cal_cnt = Reg(UInt(MAX_DW bits)) init (0)
  val cal_en = Reg(Bool()) init (false)
  flag_bits := shiftReg(2 downto 0)
  NegativeB := -PositiveB
  PositiveB := Breg.resized
  Negative2B := NegativeB |<< 1
  Positive2B := PositiveB |<< 1

  val shiftReg_low = SInt()
  val shiftReg_high = SInt()
  shiftReg_low := shiftReg(SIZEINA downto 0).asSInt
  shiftReg_high := shiftReg(SIZEINA + SIZEINB + 2 downto SIZEINA + 1).asSInt

  AddB := (shiftReg_high + PositiveB)
  Add2B := (shiftReg_high + Positive2B)
  MinusB := (shiftReg_high + NegativeB)
  Minus2B := (shiftReg_high + Negative2B)

  val cal_cnt_ov_flag = cal_cnt === MAX_NUM
  io.cal_finish := cal_cnt_ov_flag & cal_en

  when(io.din_vld) {
    cal_en := Bool(true)
  }.elsewhen(cal_cnt_ov_flag) {
    cal_en := Bool(false)
  }
  when(cal_en) {
    cal_cnt := cal_cnt + 1
  }.elsewhen(io.din_vld) {
    cal_cnt := 0
  }
  val beforeshift = Bits()
  val aftershift = Bits()
  switch(flag_bits) {
    is(0, 7) {
      beforeshift := shiftReg_high.asBits
    }
    is(1, 2) {
      beforeshift := AddB.asBits //shift and add
    }
    is(5, 6) {
      beforeshift := MinusB.asBits //shift and add
    }
    is(3) {
      beforeshift := Add2B.asBits //shift and add
    }
    is(4) {
      beforeshift := Minus2B.asBits //shift and add
    }
  }
  aftershift := ((beforeshift ## shiftReg_low.asBits).asSInt |>> 2).asBits

  //val affterAdd = Bits(SIZEINA+SIZEINB + 3 bits)
  //affterAdd :=  (beforeshift ## shiftReg_low.asBits)
  //aftershift := affterAdd.msb ## affterAdd.msb ## affterAdd( SIZEINA+SIZEINB+2 downto 2)

  when(io.din_vld) {
    shiftReg := U(0, SHIFT_SIZE - SIZEINA - 1 bits).asBits ## io.dinA.asBits ## U(0, 1 bits).asBits
    Breg := io.dinB
  }.elsewhen(cal_en) {
    shiftReg := aftershift
  }

  io.dout_vld := cal_en.fall
  io.dout := shiftReg(SIZEINA + SIZEINB downto 1).asSInt

}

object booth_signed_inst {
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
      .generate(new booth_signed())
  }.printPruned()
}