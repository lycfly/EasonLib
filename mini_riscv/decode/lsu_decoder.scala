package EasonLib.mini_riscv.decode

import EasonLib.mini_riscv.{IMM, INST_FIELD, RVIMCA}
import EasonLib.mini_riscv.lsu.decode2lsu_if
import EasonLib.mini_riscv.regfile.regfile_dual_rd_port
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable
import scala.util.Random
import scala.language.postfixOps

case class fetch2decoder_lsu(WIDTH: Int = 32) extends Bundle with IMasterSlave {
  val instr_valid = Bool()
  val instruction = Bits(WIDTH bits)
  val instr_ack = Bool()
  val instr_tag = UInt(2 bits)

  override def asMaster(): Unit = {
    out(instr_valid)
    out(instruction)
    out(instr_tag)
    in(instr_ack)
  }
}


class lsu_decoder(WIDTH: Int = 32, ADDRWD: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val fetch2lsudec = slave(fetch2decoder_lsu(WIDTH))
    val dec2lsu = master(decode2lsu_if(WIDTH,ADDRWD,REGNUM))
    val ldst_rd_port = master(regfile_dual_rd_port(WIDTH,REGNUM))
  }
  noIoPrefix()
  val dec_pool = mutable.LinkedHashMap[Bool, SpinalEnumElement[ALUOPs.type]]()
  val is_imm_type_pool = mutable.LinkedHashMap[Bool, SpinalEnumElement[ALUOPs.type]]()

  def add_instr2dec_pool(cond: Boolean, is_reg_type: Boolean, is_imm_type: Boolean, instr: Bits, Mask: MaskedLiteral, OPENUM: SpinalEnumElement[ALUOPs.type]): Bool = {
    val is_op = cond generate (instr === Mask)
    is_op match {
      case null => println("is Null")
      case a: Bool => {
        dec_pool.put(a, OPENUM)
        if (is_imm_type) {
          is_imm_type_pool.put(a, OPENUM)
        }
      }
    }
    is_op.allowPruning()
  }
  val instr = io.fetch2lsudec.instruction

  val lb = add_instr2dec_pool(true,false, true, instr, RVIMCA.LB, ALUOPs.IDLE)
  val lh = add_instr2dec_pool(true,false, true, instr, RVIMCA.LH, ALUOPs.IDLE)
  val lw = add_instr2dec_pool(true,false, true, instr, RVIMCA.LW, ALUOPs.IDLE)
  val lbu = add_instr2dec_pool(true,false, true, instr, RVIMCA.LBU, ALUOPs.IDLE)
  val lhu = add_instr2dec_pool(true,false, true, instr, RVIMCA.LHU, ALUOPs.IDLE)
  val sb = add_instr2dec_pool(true,false, true, instr, RVIMCA.SB, ALUOPs.IDLE)
  val sh = add_instr2dec_pool(true,false, true, instr, RVIMCA.SH, ALUOPs.IDLE)
  val sw = add_instr2dec_pool(true,false, true, instr, RVIMCA.SW, ALUOPs.IDLE)

  val load_s = lb | lh | lw | lbu | lhu
  val store_s = sb | sh | sw

  val itype_imm = IMM(instr).i_sext
  val stype_imm = IMM(instr).s_sext

  val imm_s = Mux(load_s, itype_imm, stype_imm)
  val rd_index_s = Mux(store_s, U(0, log2Up(REGNUM) bits) ,instr(INST_FIELD.rdRange).asUInt)

  io.ldst_rd_port.ra_index := instr(INST_FIELD.rs1Range).asUInt
  io.ldst_rd_port.rb_index := instr(INST_FIELD.rs2Range).asUInt

  val mem_size_s = Bits(2 bits)
  when(lb | lbu | sb){
    mem_size_s := 0
  }.elsewhen(lh | lhu | sh){
    mem_size_s := 1
  }.otherwise{
    mem_size_s := 2
  }

  io.dec2lsu.instr_valid.setAsReg().init(False)

  io.fetch2lsudec.instr_ack := ~io.dec2lsu.instr_valid | io.dec2lsu.instr_ack
  when(io.fetch2lsudec.instr_valid){
    io.dec2lsu.instr_valid := True
  }.elsewhen(io.dec2lsu.instr_ack){
    io.dec2lsu.instr_valid := False
  }

  io.dec2lsu.is_load.setAsReg().init(False)
  io.dec2lsu.is_store.setAsReg().init(False)
  io.dec2lsu.mem_signed.setAsReg().init(False)
  io.dec2lsu.mem_size.setAsReg().init(0)
  io.dec2lsu.lsu_wr_data.setAsReg().init(0)
  io.dec2lsu.lsu_addr.setAsReg().init(0)
  io.dec2lsu.rd_index.setAsReg().init(0)
  io.dec2lsu.instr_tag.setAsReg().init(0)

  when(io.fetch2lsudec.instr_valid & io.fetch2lsudec.instr_ack){
    io.dec2lsu.is_load := load_s
    io.dec2lsu.is_store := store_s
    io.dec2lsu.mem_signed := ~lbu & ~lhu
    io.dec2lsu.mem_size := mem_size_s
    io.dec2lsu.lsu_wr_data := B((31 downto 0) -> store_s) & io.ldst_rd_port.rb_value
    io.dec2lsu.lsu_addr := imm_s.asUInt + io.ldst_rd_port.ra_value.asUInt
    io.dec2lsu.rd_index := rd_index_s
    io.dec2lsu.instr_tag := io.fetch2lsudec.instr_tag
  }

}

object lsu_decoder_inst {
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
      .generate(new lsu_decoder())
  }.printPruned()
}