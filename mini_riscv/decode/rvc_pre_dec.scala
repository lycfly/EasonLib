package EasonLib.mini_riscv.decode

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps


class rvc_pre_dec(WIDTH: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val instr_in = in Bits (WIDTH bits)
    val rvc_predec = out(rv_predec_if(REGNUM))
  }
  noIoPrefix()
  val rvc_group0 = ~io.instr_in(1) & ~io.instr_in(0)
  val rvc_group1 = ~io.instr_in(1) & io.instr_in(0)
  val rvc_group2 = io.instr_in(1) & ~io.instr_in(0)

  val rvc_func0 = io.instr_in(15 downto 13) === 0
  val rvc_func1 = io.instr_in(15 downto 13) === 1
  val rvc_func2 = io.instr_in(15 downto 13) === 2
  val rvc_func3 = io.instr_in(15 downto 13) === 3
  val rvc_func4 = io.instr_in(15 downto 13) === 4
  val rvc_func5 = io.instr_in(15 downto 13) === 5
  val rvc_func6 = io.instr_in(15 downto 13) === 6
  val rvc_func7 = io.instr_in(15 downto 13) === 7

  val op_11_7_0 = io.instr_in(11 downto 7) === 0
  val op_6_2_0 = io.instr_in(6 downto 2) === 0
  val op_11_10_1 = io.instr_in(11 downto 10) === 3

  val is_rvc_addi4spn = rvc_group0 & rvc_func0
  val is_rvc_addi = rvc_group1 & rvc_func0
  val is_rvc_li = rvc_group1 & rvc_func2
  val is_rvc_lui = rvc_group1 & rvc_func3
  val is_rvc_arith_logic_r = rvc_group1 & rvc_func4 & op_11_10_1
  val is_rvc_arith_logic_i = rvc_group1 & rvc_func4 & ~op_11_10_1

  val is_rvc_slli = rvc_group2 & rvc_func0
  val is_rvc_mvadd = rvc_group2 & rvc_func4 & ~op_11_7_0 & ~op_6_2_0
  val is_rvc_j = rvc_group1 & rvc_func5
  val is_rvc_jal = rvc_group1 & rvc_func1
  val is_rvc_jr_base = rvc_group2 & rvc_func4 & ~op_11_7_0 & op_6_2_0
  val is_rvc_jalr = is_rvc_jr_base & io.instr_in(12)
  val is_rvc_lwsp = rvc_group2 & rvc_func2
  val is_rvc_lw = rvc_group0 & rvc_func2
  val is_rvc_swsp = rvc_group2 & rvc_func6
  val is_rvc_sw = rvc_group0 & rvc_func6

  io.rvc_predec.is_ld := is_rvc_lw | is_rvc_lwsp //C.LW, C.LWSP
  io.rvc_predec.is_st := is_rvc_sw | is_rvc_swsp //C.SW, C.SWSP
  io.rvc_predec.is_break := rvc_group2 & op_11_7_0 & op_6_2_0 & io.instr_in(12) & rvc_func4 // C.EBREAK
  io.rvc_predec.is_j := is_rvc_jr_base | is_rvc_j | is_rvc_jal //C.JALR, C.JR, C,JAL, C.J
  io.rvc_predec.is_branch := rvc_group1 & (rvc_func6 || rvc_func7.allowPruning()) //C.BEQZ, C.BNEZ

  io.rvc_predec.is_alu := is_rvc_addi4spn | is_rvc_addi | is_rvc_li | is_rvc_lui | is_rvc_arith_logic_i | is_rvc_arith_logic_r | is_rvc_slli | is_rvc_mvadd
  when(is_rvc_addi4spn || is_rvc_lwsp || is_rvc_swsp) {
    io.rvc_predec.rs1 := U"5'd2"
  }.elsewhen(is_rvc_j || is_rvc_jal){
    io.rvc_predec.rs1 := U"5'd0"
  }.elsewhen((rvc_group1 & ~io.instr_in(15)) || rvc_group2) {
    io.rvc_predec.rs1 := io.instr_in(11 downto 7).asUInt
  }.otherwise{
    io.rvc_predec.rs1 := (U"2'd1" ## io.instr_in(9 downto 7)).asUInt
  }

  when(is_rvc_arith_logic_r | is_rvc_sw){
    io.rvc_predec.rs2 := (U"2'd1" ## io.instr_in(4 downto 2)).asUInt
  }.elsewhen(rvc_group2 & io.instr_in(15)){
    io.rvc_predec.rs2 := io.instr_in(6 downto 2).asUInt
  }.otherwise{
    io.rvc_predec.rs2 := U"5'd0"
  }

  when(is_rvc_jalr || is_rvc_jal){
    io.rvc_predec.rd := U"5'd1"
  }.elsewhen((rvc_group2 & ~is_rvc_swsp) || (rvc_group1 & ~io.instr_in(15))){
    io.rvc_predec.rd := io.instr_in(11 downto 7).asUInt
  }.elsewhen(is_rvc_lw || is_rvc_addi4spn) {
    io.rvc_predec.rd := (U"2'd1" ## io.instr_in(4 downto 2)).asUInt
  }.elsewhen(rvc_group1 & rvc_func4){
    io.rvc_predec.rd := (U"2'd1" ## io.instr_in(9 downto 7)).asUInt
  }.otherwise{
    io.rvc_predec.rd := U"5'd0"
  }

  io.rvc_predec.is_div_mul := False
  io.rvc_predec.is_auipc := False
  io.rvc_predec.is_mret := False
}

object rvc_pre_dec_inst {
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
      .generate(new rvc_pre_dec())
  }.printPruned()
}