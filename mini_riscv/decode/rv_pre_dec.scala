package EasonLib.mini_riscv.decode

import EasonLib.mini_riscv.INST_FIELD
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps
case class rv_predec_if(REGMEM: Int = 32) extends Bundle {
  val rs1 = UInt(log2Up(REGMEM) bits)
  val rs2 = UInt(log2Up(REGMEM) bits)
  val rd = UInt(log2Up(REGMEM) bits)
  val is_ld = Bool()
  val is_st = Bool()
  val is_alu = Bool()
  val is_div_mul = Bool().allowPruning()
  val is_break = Bool().allowPruning()
  val is_j = Bool().allowPruning()
  val is_branch = Bool().allowPruning()
  val is_auipc = Bool().allowPruning()
  val is_mret = Bool().allowPruning()
}
class rv_pre_dec(WIDTH: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val instr_in = in Bits(WIDTH bits)
    val rv_predec = out(rv_predec_if(REGNUM))

  }
  noIoPrefix()
  val op_s = io.instr_in(INST_FIELD.opRange)
  val rd_s = io.instr_in(INST_FIELD.rdRange)
  val funt3_s = io.instr_in(INST_FIELD.funct3Range)
  val rs1_s = io.instr_in(INST_FIELD.rs1Range)
  val rs2_s = io.instr_in(INST_FIELD.rs2Range)
  val func7_s = io.instr_in(INST_FIELD.funct7Range)

  val is_lui = op_s === B"7'b0110111"
  val is_auipc = op_s === B"7'b0010111"
  val is_jal = op_s === B"7'b1101111"
  val is_jalr = op_s === B"7'b1100111"

  val is_branch = op_s === B"7'b1100011"
  val is_load = op_s === B"7'b0000011"
  val is_store = op_s === B"7'b0100011"
  val is_alu_imm = op_s === B"7'b0010011"
  val is_alu_reg = op_s === B"7'b0110011"
  val is_system = op_s === B"7'b1110011"

  val is_break = is_system & (funt3_s === B"3'b000") & (rs1_s === B"5'b00001") & (func7_s === B"7'b0000000")
  val is_mret = is_system & (funt3_s === B"3'b000") & (rs1_s === B"5'b00010") & (func7_s === B"7'b0011000")
  val is_csrrw = is_system & (funt3_s === B"3'b001")

  io.rv_predec.rs1 := (is_lui | is_auipc | is_jal | is_mret | is_break) ? U"5'd0" | rs1_s.asUInt
  io.rv_predec.rs2 := (is_branch | is_store | is_alu_reg) ? rs2_s.asUInt | U"5'd0"
  io.rv_predec.rd := (is_branch | is_store | is_break | is_mret) ? U"5'd0" | rd_s.asUInt

  io.rv_predec.is_ld := is_load
  io.rv_predec.is_st := is_store
  io.rv_predec.is_alu := is_alu_imm | is_alu_reg | is_lui | is_csrrw
  io.rv_predec.is_div_mul := is_alu_reg & func7_s(0)

  io.rv_predec.is_break := is_break
  io.rv_predec.is_j := is_jal | is_jalr.allowPruning()
  io.rv_predec.is_branch := is_branch
  io.rv_predec.is_auipc := is_auipc
  io.rv_predec.is_mret := is_mret

}

object rv_pre_dec_inst {
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
      .generate(new rv_pre_dec())
  }.printPruned()
}