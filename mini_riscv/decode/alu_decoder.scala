package EasonLib.mini_riscv.decode

import EasonLib.mini_riscv.{BIT_MANIPULATION, IMM, INST_FIELD, RVIMCA}
import EasonLib.mini_riscv.regfile.regfile_dual_rd_port
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps
import mini_riscv._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ALUOPs extends SpinalEnum {
  val IDLE, ADD, SUB, AND, OR, XOR, SLT, SLTU, SHL, SHR, MULL, MULH, DIV, REM, NPC, AUIPC, RA = newElement()
  val SH1ADD, SH2ADD, SH3ADD = newElement()
  val ANDN, CLZ, CPOP, CTZ, MAX, MAXU, MIN, MINU, ORC_B, ORN, REV8, ROL, ROR, SEXT_B, SEXT_H, XNOR, ZEXT_H = newElement()
  val CLMUL, CLMULH, CLMULR = newElement()
  val BCLR, BEXT, BINV, BSET = newElement()
}

//case class ALUOPs(USE_ZBA: Boolean = false,
//                  USE_ZBB: Boolean = false,
//                  USE_ZBC: Boolean = false,
//                  USE_ZBS: Boolean = false) extends SpinalEnum {
//  val ADD, SUB, AND, OR, XOR, SLT, SLTU, SHL, SHR, MULL, MULH, DIV, REM, NPC, AUIPC, RA = newElement()
//  (USE_ZBA) generate {
//    val SH1ADD, SH2ADD, SH3ADD = newElement()
//  }
//  (USE_ZBB) generate{
//    val ANDN, CLZ, CPOP, CTZ, MAX, MAXU, MIN, MINU, ORC_B, ORN, REV8, ROL, ROR, RORI, SEXT_B, SEXT_H, XNOR, ZEXT_H = newElement()
//  }
//  (USE_ZBC) generate{
//    val CLMUL, CLMULH, CLMULR = newElement()
//  }
//  (USE_ZBS) generate{
//    val BCLR, BCLRI, BEXT, BEXTI, BINV, BINVI, BSET, BSETI = newElement()
//  }
//}
object BRANCH_TYPE extends SpinalEnum {
  val BR_NONE, BR_EQ, BR_NE, BR_LT, BR_GE, BR_LTU, BR_GEU, BR_JUMP = newElement()
}

case class fetch2decoder_alu(WIDTH: Int = 32, ADDRWD: Int = 32) extends Bundle with IMasterSlave {
  val instr_valid = Bool()
  val instr_type = Bool() // 0: 16bit, 1: 32bit
  val instruction = Bits(WIDTH bits)
  val pc_of_instr = UInt(ADDRWD bits)
  val instr_ack = Bool()

  override def asMaster(): Unit = {
    out(instr_valid)
    out(instr_type)
    out(instruction)
    out(pc_of_instr)
    in(instr_ack)
  }
}

case class dec2alu_if(WIDTH: Int = 32, ADDRWD: Int = 32) extends Bundle with IMasterSlave {
  val instr_valid = Bool()
  val use_imm = Bool()
  val imm_value = Bits(WIDTH bits)
  val ra_value = Bits(WIDTH bits)
  val rb_value = Bits(WIDTH bits)
  val ra_is_signed = Bool()
  val rb_is_signed = Bool()
  val branch_type = BRANCH_TYPE()
  val is_reg_jump = Bool()
  val is_break = Bool()
  val is_irq = Bool()
  val is_mret = Bool()

  val rd_index = UInt(log2Up(WIDTH) bits)
  val is_csrrw = Bool()
  val csr_addr = UInt(12 bits)

  val pc = UInt(ADDRWD bits)
  val next_pc = UInt(ADDRWD bits)

  val alu_optype = ALUOPs()

  val instr_ack = Bool()

  override def asMaster(): Unit = {
    this.flatten.foreach(a => out(a))
    in(instr_ack)
  }
}

class alu_decoder(USE_I: Boolean = true,
                  USE_M: Boolean = true,
                  USE_ZBA: Boolean = true,
                  USE_ZBB: Boolean = true,
                  USE_ZBC: Boolean = false,
                  USE_ZBS: Boolean = true,

                  WIDTH: Int = 32, ADDRWD: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val fetch2dec_alu = slave(fetch2decoder_alu(WIDTH, ADDRWD))
    val dec2alu = master(dec2alu_if(WIDTH, ADDRWD))
    val alu_rd_port = master(regfile_dual_rd_port(WIDTH,REGNUM))
    val csr_value = out Bits(WIDTH bits)

  }
  noIoPrefix()
  val instr = io.fetch2dec_alu.instruction
  val RANG = INST_FIELD
  val rd_s = instr(RANG.rdRange)
  val ra_s = instr(RANG.rs1Range)
  val rb_s = instr(RANG.rs2Range)
  val csr_s = instr(RANG.csrRange)
  //  val op_s = instr(RANG.opRange)
  //  val funct3_s = instr(RANG.funct3Range)
  //  val funct7_s = instr(RANG.funct7Range)

  //  val op_is_branch_s = op_s === OP_TYPE.branch
  //  val op_is_alu_imm_s = op_s === OP_TYPE.alu_imm
  //  val op_is_alu_reg_s = op_s === OP_TYPE.alu_reg
  //  val op_is_system_s = op_s === OP_TYPE.system

  val dec_pool = mutable.LinkedHashMap[Bool, SpinalEnumElement[ALUOPs.type]]()
  val is_reg_type_pool = mutable.LinkedHashMap[Bool, SpinalEnumElement[ALUOPs.type]]()
  val is_imm_type_pool = mutable.LinkedHashMap[Bool, SpinalEnumElement[ALUOPs.type]]()

  def add_instr2dec_pool( cond: Boolean, is_reg_type: Boolean,is_imm_type: Boolean, instr: Bits, Mask: MaskedLiteral,
                          OPENUM: SpinalEnumElement[ALUOPs.type], addpool:Boolean = true): Bool = {
    val is_op = cond generate (instr === Mask)
    is_op match {
      case null => println("is Null")
      case a: Bool => {
        a.allowPruning()
        if(addpool) {
          dec_pool.put(a, OPENUM)
        }
        if (is_reg_type) {
          is_reg_type_pool.put(a, OPENUM)
        }
        else if (is_imm_type) {
          is_imm_type_pool.put(a, OPENUM)
        }

      }
    }
    is_op
  }


  /** I,M */
  val lui = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.LUI, ALUOPs.ADD)
  val auipc = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.AUIPC, ALUOPs.AUIPC)
  val jal = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.JAL(false), ALUOPs.NPC)
  val jalr = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.JALR, ALUOPs.NPC)

  val beq = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.BEQ(false), ALUOPs.IDLE, addpool = false)
  val bne = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.BNE(false), ALUOPs.IDLE, addpool = false)
  val blt = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.BLT(false), ALUOPs.IDLE, addpool = false)
  val bge = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.BGE(false), ALUOPs.IDLE, addpool = false)
  val bltu = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.BLTU(false), ALUOPs.IDLE, addpool = false)
  val bgeu = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.BGEU(false), ALUOPs.IDLE, addpool = false)

  val addi = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.ADDI, ALUOPs.ADD)
  val slti = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.SLTI, ALUOPs.SLT)
  val sltiu = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.SLTIU, ALUOPs.SLTU)
  val xori = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.XORI, ALUOPs.XOR)
  val ori = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.ORI, ALUOPs.OR)
  val andi = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.ANDI, ALUOPs.AND)
  val slli = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.SLLI, ALUOPs.SHL)
  val srli = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.SRLI, ALUOPs.SHR)
  val srai = add_instr2dec_pool(USE_I,false, true, instr, RVIMCA.SRAI, ALUOPs.SHR)

  val add = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.ADD, ALUOPs.ADD)
  val sub = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.SUB, ALUOPs.SUB)
  val slt = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.SLT, ALUOPs.SLT)
  val sltu = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.SLTU, ALUOPs.SLTU)
  val xor = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.XOR, ALUOPs.XOR)
  val or = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.OR, ALUOPs.OR)
  val and = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.AND, ALUOPs.AND)
  val sll = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.SLL, ALUOPs.SHL)
  val srl = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.ADD, ALUOPs.SHR)
  val sra = add_instr2dec_pool(USE_I,true, false, instr, RVIMCA.SRA, ALUOPs.SHR)

  val mul = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.MUL, ALUOPs.MULL)
  val mulh = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.MULH, ALUOPs.MULH)
  val mulhsu = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.MULHSU, ALUOPs.IDLE)
  val mulhu = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.MULHU, ALUOPs.IDLE)
  val div = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.DIV, ALUOPs.DIV)
  val divu = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.DIVU, ALUOPs.DIV)
  val rem = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.REM, ALUOPs.REM)
  val remu = add_instr2dec_pool(USE_M,true, false, instr, RVIMCA.REMU, ALUOPs.REM)

  val break = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.EBREAK, ALUOPs.IDLE, addpool = false)
  val csrrw = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.CSRRW, ALUOPs.RA, addpool = true)
  val mret = add_instr2dec_pool(USE_I,false, false, instr, RVIMCA.MRET, ALUOPs.IDLE, addpool = false)

  /** ZBA */
  val sh1add = add_instr2dec_pool(USE_ZBA,true, false, instr, BIT_MANIPULATION.SH1ADD, ALUOPs.SH1ADD)
  val sh2add = add_instr2dec_pool(USE_ZBA,true, false, instr, BIT_MANIPULATION.SH2ADD, ALUOPs.SH2ADD)
  val sh3add = add_instr2dec_pool(USE_ZBA,true, false, instr, BIT_MANIPULATION.SH3ADD, ALUOPs.SH3ADD)
  /** ZBB */
  val andn = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.ANDN, ALUOPs.AND)
  val clz = add_instr2dec_pool(USE_ZBB,false, true, instr, BIT_MANIPULATION.CLZ, ALUOPs.CLZ)
  val cpop = add_instr2dec_pool(USE_ZBB,false, true, instr, BIT_MANIPULATION.CPOP, ALUOPs.CPOP)
  val ctz = add_instr2dec_pool(USE_ZBB,false, true, instr, BIT_MANIPULATION.CTZ, ALUOPs.CTZ)
  val max = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.MAX, ALUOPs.MAX)
  val maxu = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.MAXU, ALUOPs.MAXU)
  val min = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.MIN, ALUOPs.MIN)
  val minu = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.MINU, ALUOPs.MINU)
  val orc_b = add_instr2dec_pool(USE_ZBB,false, true, instr, BIT_MANIPULATION.ORC_B, ALUOPs.ORC_B)
  val orn = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.ORN, ALUOPs.ORN)
  val rev8 = add_instr2dec_pool(USE_ZBB,false, true, instr, BIT_MANIPULATION.REV8, ALUOPs.REV8)
  val rol = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.ROL, ALUOPs.ROL)
  val ror = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.ROR, ALUOPs.ROR)
  val rori = add_instr2dec_pool(USE_ZBB,false, true, instr, BIT_MANIPULATION.RORI, ALUOPs.ROR)
  val sext_b = add_instr2dec_pool(USE_ZBB,false, true,  instr, BIT_MANIPULATION.SEXT_B, ALUOPs.SEXT_B)
  val sext_h = add_instr2dec_pool(USE_ZBB,false, true,  instr, BIT_MANIPULATION.SEXT_H, ALUOPs.SEXT_H)
  val xnor = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.XNOR, ALUOPs.XNOR)
  val zext_h = add_instr2dec_pool(USE_ZBB,true, false, instr, BIT_MANIPULATION.ZEXT_H, ALUOPs.ZEXT_H)
  /** ZBC */
  val clmul = add_instr2dec_pool(USE_ZBC,true, false, instr, BIT_MANIPULATION.CLMUL, ALUOPs.CLMUL)
  val clmulh = add_instr2dec_pool(USE_ZBC,true, false, instr, BIT_MANIPULATION.CLMULH, ALUOPs.CLMULH)
  val clmulr = add_instr2dec_pool(USE_ZBC,true, false, instr, BIT_MANIPULATION.CLMULR, ALUOPs.CLMULR)
  /** ZBS */
  val bclr = add_instr2dec_pool(USE_ZBS,true, false, instr, BIT_MANIPULATION.BCLR, ALUOPs.BCLR)
  val bclri = add_instr2dec_pool(USE_ZBS,false, true,  instr, BIT_MANIPULATION.BCLRI, ALUOPs.BCLR)
  val bext = add_instr2dec_pool(USE_ZBS,true, false, instr, BIT_MANIPULATION.BEXT, ALUOPs.BEXT)
  val bexti = add_instr2dec_pool(USE_ZBS,false, true,  instr, BIT_MANIPULATION.BEXTI, ALUOPs.BEXT)
  val binv = add_instr2dec_pool(USE_ZBS,true, false, instr, BIT_MANIPULATION.BINV, ALUOPs.BINV)
  val binvi = add_instr2dec_pool(USE_ZBS,false, true,  instr, BIT_MANIPULATION.BINVI, ALUOPs.BINV)
  val bset = add_instr2dec_pool(USE_ZBS,true, false, instr, BIT_MANIPULATION.BSET, ALUOPs.BSET)
  val bseti = add_instr2dec_pool(USE_ZBS,false, true,  instr, BIT_MANIPULATION.BSETI, ALUOPs.BSET)

  val aluop = ALUOPs()

  aluop := ALUOPs.IDLE
  for ((key, ops) <- dec_pool) {
    when(key) {
      aluop := ops
    }
  }

  val alu_reg_s = is_reg_type_pool.keys.reduceBalancedTree(_ | _)
  val alu_imm_s = is_imm_type_pool.keys.reduceBalancedTree(_ | _)

  val branch_s = beq | bne | blt | bge | bltu | bgeu
  val jump_s = jal | jalr
  val system_s = break | csrrw | mret

  val itype_imm = IMM(instr).i_sext
  val stype_imm = IMM(instr).s_sext
  val btype_imm = IMM(instr).b_sext
  val utype_imm = IMM(instr).u
  val jtype_imm = IMM(instr).j_sext

  val imm_s = Bits(WIDTH bits)
  when(lui | auipc){
    imm_s := utype_imm
  }.elsewhen(branch_s){
    imm_s := btype_imm
  }.elsewhen(jalr | alu_imm_s){
    imm_s := itype_imm
  }.elsewhen(jal){
    imm_s := jtype_imm
  }.otherwise{
    imm_s := 0
  }

  val rd_index_s = Mux(branch_s, U(0, log2Up(REGNUM) bits), rd_s.asUInt)
  val ra_index_s = Mux(lui | auipc | jal, U(0, log2Up(REGNUM) bits), ra_s.asUInt)
  val rb_index_s = Mux( jump_s | alu_imm_s, U(0, log2Up(REGNUM) bits), rb_s.asUInt)

  val csr_addr_s = Mux(csrrw, csr_s.asUInt,  U(0, 12 bits))


  val branch_type = BRANCH_TYPE()
  when(beq){
    branch_type :=BRANCH_TYPE.BR_EQ
  }.elsewhen(bne) {
    branch_type :=BRANCH_TYPE.BR_NE
  }.elsewhen(blt) {
    branch_type := BRANCH_TYPE.BR_LT
  }.elsewhen(bge) {
    branch_type := BRANCH_TYPE.BR_GE
  }.elsewhen(bltu) {
    branch_type := BRANCH_TYPE.BR_LTU
  }.elsewhen(bgeu) {
    branch_type := BRANCH_TYPE.BR_GEU
  }.elsewhen(jump_s) {
    branch_type := BRANCH_TYPE.BR_JUMP
  }.otherwise{
    branch_type := BRANCH_TYPE.BR_NONE
  }

  io.dec2alu.flatten.foreach(
    a => {
      if(a.isOutput){
        a.setAsReg() init(a.getZero)
      }
    }
  )
  io.fetch2dec_alu.instr_ack := ~io.dec2alu.instr_valid | io.dec2alu.instr_ack
  when(io.fetch2dec_alu.instr_valid) {
    io.dec2alu.instr_valid := True
  }.elsewhen(io.dec2alu.instr_ack) {
    io.dec2alu.instr_valid := False
  }

  when(io.fetch2dec_alu.instr_valid & io.fetch2dec_alu.instr_ack){
    io.dec2alu.use_imm := alu_imm_s | jal
    io.dec2alu.imm_value := imm_s
    io.dec2alu.rb_value := Mux(csrrw, csr_s.asUInt.resize(WIDTH).asBits, io.alu_rd_port.rb_value)
    io.dec2alu.ra_value := io.alu_rd_port.ra_value
    io.dec2alu.alu_optype := aluop
    io.dec2alu.ra_is_signed := mulh | mulhsu | div | rem | sra | srai
    io.dec2alu.rb_is_signed := mulh | div | rem | andn | orn | xnor    //use inverted logic
    io.dec2alu.branch_type := branch_type
    io.dec2alu.is_reg_jump := jalr
    io.dec2alu.rd_index := rd_index_s
    io.dec2alu.is_csrrw := csrrw
    io.dec2alu.csr_addr := csr_addr_s
    io.dec2alu.pc := io.fetch2dec_alu.pc_of_instr - (Mux(io.fetch2dec_alu.instr_type, U(4), U(2)))
    io.dec2alu.next_pc := io.fetch2dec_alu.pc_of_instr
  }

  io.alu_rd_port.ra_index := ra_index_s
  io.alu_rd_port.rb_index := rb_index_s

  io.csr_value := csr_s.asUInt.resize(WIDTH).asBits

}

object alu_decoder_inst {
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
      .generate(new alu_decoder())
  }.printPruned()
}