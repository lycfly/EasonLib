package EasonLib.mini_riscv.alu

import EasonLib.Arithmetic.divider.restoring_div
import EasonLib.Arithmetic.{barrel_shift, logic_unit}
import EasonLib.Common_ip.{lzc, popcount}
import EasonLib.mini_riscv.csr.{csr2alu_if, write_csr_if}
import EasonLib.mini_riscv.decode.{ALUOPs, BRANCH_TYPE, dec2alu_if}
import EasonLib.mini_riscv.regfile.regfile_wr_port
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps

case class branch_info(ADDRWD: Int) extends Bundle with IMasterSlave {
  val branch_taken = Bool()
  val branch_not_taken = Bool()
  val jump_addr = UInt(ADDRWD bits)

  override def asMaster(): Unit = {
    out(branch_taken)
    out(branch_not_taken)
    out(jump_addr)
  }
}

case class FunctionUnit(op: SpinalEnumCraft[ALUOPs.type]) {
  val func_list = mutable.LinkedHashMap[Bool, (Array[Data], Data)]()
  var need_generate = false

  def regist_func(cond: Boolean, OPENUM: SpinalEnumElement[ALUOPs.type], input_route: Array[Data], output_exp: Data): Bool = {
    val is_op = cond generate (op === OPENUM)
    is_op match {
      case null => println("is Null")
      case a: Bool => {
        need_generate = true
        a.allowPruning()
        if (cond) {
          func_list.put(a, (input_route, output_exp))
        }
      }
    }
    is_op
  }

  def route(unit_componet: Component): Unit = {
    val allinputs = get_unit_all_inputs(unit_componet)
    allinputs.foreach(a => a := a.getZero)

    for ((key, tup) <- func_list) {
      var i = 0
      when(key) {
        allinputs.foreach(a => {
          //          println(tup._1(i))
          a := tup._1(i).asInstanceOf[a.type]
          i += 1
        })
      }
    }
  }

  def get_unit_all_inputs(componet: Component): mutable.ArrayBuffer[BaseType] = {
    val all_inputs = mutable.ArrayBuffer[BaseType]()
    componet.getAllIo.foreach(a => {
      if (a.isInput) {
        all_inputs += a
      }
    })
    all_inputs
  }


}

class alu(USE_M: Boolean = true,
          USE_ZBA: Boolean = true,
          USE_ZBB: Boolean = true,
          USE_ZBC: Boolean = false,
          USE_ZBS: Boolean = true,
          MULTI_CYCLE_MUL: Boolean = true,
          WIDTH: Int = 32, ADDRWD: Int = 32, REGNUM: Int = 32, CSRWIDTH: Int = 32) extends Component {
  val io = new Bundle {
    val dec2alu = slave(dec2alu_if(WIDTH, ADDRWD))
    val alu_wr_port = master(regfile_wr_port(WIDTH, REGNUM))
    val alu_wr_csr = master(write_csr_if(CSRWIDTH))
    val alu2fetch_brInfo = master(branch_info(ADDRWD))
    val csr2alu = slave(csr2alu_if(CSRWIDTH))

  }
  noIoPrefix()
  val reslist_all = ArrayBuffer[mutable.LinkedHashMap[Bool, (Array[Data], Data)]]()

  val alu_opb_s = io.dec2alu.use_imm ? io.dec2alu.imm_value | io.dec2alu.rb_value
  val shifter_res = Bits(WIDTH bits)

  /**
   * Adder
   */
  val adder_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val adder_unit = new adder(WIDTH)

  val adder_result = adder_unit.io.dout
  val adder_cout = adder_unit.io.cout
  val adder_res_neg_flag = adder_unit.io.neg_flag
  val adder_res_ov_flag = adder_unit.io.ovflow_flag
  val adder_res_zero_flag = adder_unit.io.zero_flag

  val is_add = adder_unit_helper.regist_func(true, ALUOPs.ADD, Array(True, False, io.dec2alu.ra_value, alu_opb_s), adder_result)
  val is_sub = adder_unit_helper.regist_func(true, ALUOPs.SUB, Array(True, True, io.dec2alu.ra_value, alu_opb_s), adder_result)
  val is_slt = adder_unit_helper.regist_func(true, ALUOPs.SLT, Array(True, True, io.dec2alu.ra_value, alu_opb_s),
    ((adder_res_neg_flag ^ adder_res_ov_flag) ? B(1, WIDTH bits) | B(0, WIDTH bits)))
  val is_sltu = adder_unit_helper.regist_func(true, ALUOPs.SLTU, Array(True, True, io.dec2alu.ra_value, alu_opb_s),
    (adder_cout ? B(0, WIDTH bits) | B(1, WIDTH bits)))
  val is_sh1add = adder_unit_helper.regist_func(USE_ZBA, ALUOPs.SH1ADD, Array(True, False, shifter_res, alu_opb_s), adder_result)
  val is_sh2add = adder_unit_helper.regist_func(USE_ZBA, ALUOPs.SH2ADD, Array(True, False, shifter_res, alu_opb_s), adder_result)
  val is_sh3add = adder_unit_helper.regist_func(USE_ZBA, ALUOPs.SH3ADD, Array(True, False, shifter_res, alu_opb_s), adder_result)

  val is_min = adder_unit_helper.regist_func(USE_ZBB, ALUOPs.MIN, Array(True, True, io.dec2alu.ra_value, alu_opb_s),
    ((adder_res_neg_flag ^ adder_res_ov_flag) ? io.dec2alu.ra_value | io.dec2alu.rb_value))
  val is_minu = adder_unit_helper.regist_func(USE_ZBB, ALUOPs.MINU, Array(True, True, io.dec2alu.ra_value, alu_opb_s),
    (adder_cout ? io.dec2alu.rb_value | io.dec2alu.ra_value))
  val is_max = adder_unit_helper.regist_func(USE_ZBB, ALUOPs.MAX, Array(True, True, io.dec2alu.ra_value, alu_opb_s),
    ((adder_res_neg_flag ^ adder_res_ov_flag) ? io.dec2alu.rb_value | io.dec2alu.ra_value))
  val is_maxu = adder_unit_helper.regist_func(USE_ZBB, ALUOPs.MAXU, Array(True, True, io.dec2alu.ra_value, alu_opb_s),
    (adder_cout ? io.dec2alu.ra_value | io.dec2alu.rb_value))

  adder_unit_helper.route(adder_unit)
  reslist_all += adder_unit_helper.func_list

  /**
   * byte operator
   */
  val byte_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val orcb_res = Bits(WIDTH bits)
  val rev8_res = Bits(WIDTH bits)
  val is_sextb = byte_unit_helper.regist_func(USE_ZBB, ALUOPs.SEXT_B, Array(), io.dec2alu.ra_value(7 downto 0).asSInt.resize(WIDTH).asBits)
  val is_sexth = byte_unit_helper.regist_func(USE_ZBB, ALUOPs.SEXT_H, Array(), io.dec2alu.ra_value(15 downto 0).asSInt.resize(WIDTH).asBits)
  val is_zexth = byte_unit_helper.regist_func(USE_ZBB, ALUOPs.ZEXT_H, Array(), io.dec2alu.ra_value(15 downto 0).resize(WIDTH))
  val is_orcb = byte_unit_helper.regist_func(USE_ZBB, ALUOPs.ORC_B, Array(), orcb_res)
  val is_rev8 = byte_unit_helper.regist_func(USE_ZBB, ALUOPs.ZEXT_H, Array(), rev8_res)

  if (is_orcb == null) {
    orcb_res.allowPruning() := 0
  } else {
    val partnum = (WIDTH / 8)
    for (i <- 0 until partnum) {
      orcb_res((i + 1) * 8 - 1 downto i * 8).setAllTo(io.dec2alu.ra_value((i + 1) * 8 - 1 downto i * 8).orR)
    }
  }
  if (is_rev8 == null) {
    rev8_res.allowPruning() := 0
  } else {
    val partnum = (WIDTH / 8)
    for (i <- 0 until partnum) {
      rev8_res((i + 1) * 8 - 1 downto i * 8) := io.dec2alu.ra_value((partnum - i) * 8 - 1 downto (partnum - i - 1) * 8)
    }
  }

  reslist_all += byte_unit_helper.func_list

  /**
   * logic op
   */
  val bitlogic_unit = new logic_unit(WIDTH)
  val bitlogic_res = bitlogic_unit.io.data_out
  val bitlogic_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_or = bitlogic_unit_helper.regist_func(true, ALUOPs.OR, Array(True, B(0, 2 bits), False, False, io.dec2alu.ra_value, io.dec2alu.rb_value), bitlogic_res)
  val is_and = bitlogic_unit_helper.regist_func(true, ALUOPs.AND, Array(True, B(1, 2 bits), False, False, io.dec2alu.ra_value, io.dec2alu.rb_value), bitlogic_res)
  val is_xor = bitlogic_unit_helper.regist_func(true, ALUOPs.XOR, Array(True, B(2, 2 bits), False, False, io.dec2alu.ra_value, io.dec2alu.rb_value), bitlogic_res)
  val is_orn = bitlogic_unit_helper.regist_func(USE_ZBB, ALUOPs.ORN, Array(True, B(0, 2 bits), False, True, io.dec2alu.ra_value, io.dec2alu.rb_value), bitlogic_res)
  val is_andn = bitlogic_unit_helper.regist_func(USE_ZBB, ALUOPs.ANDN, Array(True, B(1, 2 bits), False, True, io.dec2alu.ra_value, io.dec2alu.rb_value), bitlogic_res)
  val is_xnor = bitlogic_unit_helper.regist_func(USE_ZBB, ALUOPs.XNOR, Array(True, B(2, 2 bits), False, True, io.dec2alu.ra_value, io.dec2alu.rb_value), bitlogic_res)

  bitlogic_unit_helper.route(bitlogic_unit)
  reslist_all += bitlogic_unit_helper.func_list

  /**
   * popcount
   */
  val popcount_res = UInt(log2Up(WIDTH) + 1 bits)
  val popcount_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_cpop = popcount_unit_helper.regist_func(USE_ZBB, ALUOPs.CPOP, Array(True, io.dec2alu.ra_value), popcount_res.resize(WIDTH))
  if (popcount_unit_helper.need_generate) {
    val popcount_unit = new popcount(WIDTH)
    popcount_unit_helper.route(popcount_unit)
    popcount_res := popcount_unit.io.cnt_out
    reslist_all += popcount_unit_helper.func_list

  }
  else {
    popcount_res.allowPruning() := 0
  }

  /**
   * lzc
   */
  val lzc_res = UInt(log2Up(WIDTH) + 1 bits)
  val lzc_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_ctz = lzc_unit_helper.regist_func(USE_ZBB, ALUOPs.CTZ, Array(False, False, True, io.dec2alu.ra_value), lzc_res.resize(WIDTH))
  val is_clz = lzc_unit_helper.regist_func(USE_ZBB, ALUOPs.CLZ, Array(False, True, False, io.dec2alu.ra_value), lzc_res.resize(WIDTH))
  if (lzc_unit_helper.need_generate) {
    val lzc_unit = new lzc(WIDTH)
    lzc_unit_helper.route(lzc_unit)
    lzc_res := lzc_unit.io.cnt_out
    reslist_all += lzc_unit_helper.func_list
  }
  else {
    lzc_res.allowPruning() := 0
  }
  /**
   * Shifter
   */
  val shift_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_shl = shift_unit_helper.regist_func(true, ALUOPs.SHL, Array(False, True, True, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, io.dec2alu.ra_value), shifter_res)
  val shift_is_sh1add = shift_unit_helper.regist_func(USE_ZBA, ALUOPs.SH1ADD, Array(False, True, False, U(1, log2Up(WIDTH) bits), io.dec2alu.ra_value), null.asInstanceOf[Data])
  val shift_is_sh2add = shift_unit_helper.regist_func(USE_ZBA, ALUOPs.SH2ADD, Array(False, True, False, U(2, log2Up(WIDTH) bits), io.dec2alu.ra_value), null.asInstanceOf[Data])
  val shift_is_sh3add = shift_unit_helper.regist_func(USE_ZBA, ALUOPs.SH3ADD, Array(False, True, False, U(3, log2Up(WIDTH) bits), io.dec2alu.ra_value), null.asInstanceOf[Data])
  val is_rol = shift_unit_helper.regist_func(USE_ZBB, ALUOPs.ROL, Array(True, True, False, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, io.dec2alu.ra_value), shifter_res)
  val is_ror = shift_unit_helper.regist_func(USE_ZBB, ALUOPs.ROR, Array(True, False, False, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, io.dec2alu.ra_value), shifter_res)

  val is_bext = shift_unit_helper.regist_func(USE_ZBS, ALUOPs.BEXT, Array(False, False, False, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, io.dec2alu.ra_value), shifter_res(0).asBits.resize(WIDTH))
  val is_bclr = shift_unit_helper.regist_func(USE_ZBS, ALUOPs.BCLR, Array(False, True, False, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, B(0, WIDTH bits)), ~shifter_res & io.dec2alu.ra_value)
  val is_binv = shift_unit_helper.regist_func(USE_ZBS, ALUOPs.BINV, Array(False, True, False, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, B(0, WIDTH bits)), shifter_res ^ io.dec2alu.ra_value)
  val is_bset = shift_unit_helper.regist_func(USE_ZBS, ALUOPs.BSET, Array(False, True, False, alu_opb_s(log2Up(WIDTH) - 1 downto 0).asUInt, B(0, WIDTH bits)), shifter_res | io.dec2alu.ra_value)

  if (shift_unit_helper.need_generate) {
    val shift_unit = new barrel_shift(WIDTH)
    shift_unit_helper.route(shift_unit)
    shifter_res := shift_unit.io.data_out
    reslist_all += shift_unit_helper.func_list
  }
  else {
    shifter_res := 0
  }

  /**
   * Multiplier
   * if MULTI_CYCLE_MUL = 0, use native combinational multiplier
   * else use booth4 multiplier (for 32b * 32b need 17 cycles)
   */
  val mul_cause_stall = Bool()
  val mul_dout = Bits(WIDTH * 2 bits)
  val mul_dout_vld = Bool()
  val mul_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_mull = mul_unit_helper.regist_func(USE_M, ALUOPs.MULL, Array(True, io.dec2alu.ra_is_signed, io.dec2alu.rb_is_signed, io.dec2alu.ra_value, alu_opb_s),
    mul_dout(WIDTH - 1 downto 0))
  val is_mulh = mul_unit_helper.regist_func(USE_M, ALUOPs.MULH, Array(True, io.dec2alu.ra_is_signed, io.dec2alu.rb_is_signed, io.dec2alu.ra_value, alu_opb_s),
    mul_dout(WIDTH * 2 - 1 downto WIDTH))
  if (mul_unit_helper.need_generate) {
    val mul_unit = new mul_wrapper(WIDTH = WIDTH, MULTI_CYCLE_MUL = MULTI_CYCLE_MUL)
    mul_unit_helper.route(mul_unit)
    mul_dout_vld := mul_unit.io.dout_vld
    mul_dout := mul_dout_vld ? mul_unit.io.dout | 0
    mul_cause_stall := mul_unit.io.busy
    reslist_all += mul_unit_helper.func_list

  }
  else {
    mul_cause_stall.allowPruning() := False
    mul_dout.allowPruning() := 0
    mul_dout_vld.allowPruning() := False
  }

  /**
   * Divider
   * The quotient of division by zero has all bits set, and the remainder of division by zero equals the dividend.
   * Signed division overflow occurs only when the most-negative integer is divided by -1.
   * The quotient of a signed division with overflow is equal to the dividend, and the remainder is zero.
   * Unsigned division overflow cannot occur.
   */
  val div_cause_stall = Bool()
  val div_dout_vld = Bool()
  val quot = Bits(WIDTH bits)
  val remainder = Bits(WIDTH bits)
  val div_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_div = div_unit_helper.regist_func(USE_M, ALUOPs.DIV, Array(True, io.dec2alu.ra_is_signed & io.dec2alu.rb_is_signed, io.dec2alu.ra_value, io.dec2alu.rb_value), quot)
  val is_rem = div_unit_helper.regist_func(USE_M, ALUOPs.REM, Array(True, io.dec2alu.ra_is_signed & io.dec2alu.rb_is_signed, io.dec2alu.ra_value, io.dec2alu.rb_value), remainder)
  if (div_unit_helper.need_generate) {
    val div_unit = new div_wrapper(WIDTH, WIDTH)
    div_unit_helper.route(div_unit)
    quot := div_dout_vld ? div_unit.io.quot | 0
    remainder := div_dout_vld ? div_unit.io.remainder | 0
    div_dout_vld := div_unit.io.dout_vld
    div_cause_stall :=  div_unit.io.busy
    reslist_all += div_unit_helper.func_list

  }
  else {
    div_cause_stall.allowPruning() := False
    quot.allowPruning() := 0
    remainder.allowPruning() := 0
    div_dout_vld.allowPruning() := False
  }

  val jump_addr_s = UInt(ADDRWD bits)

  val branch_unit_helper = FunctionUnit(io.dec2alu.alu_optype)
  val is_npc = branch_unit_helper.regist_func(true, ALUOPs.NPC, Array(), io.dec2alu.next_pc.asBits)
  val is_auipc = branch_unit_helper.regist_func(true, ALUOPs.AUIPC, Array(), jump_addr_s.asBits)
  val is_ra = branch_unit_helper.regist_func(true, ALUOPs.RA, Array(), io.dec2alu.ra_value)
  reslist_all += branch_unit_helper.func_list


  val alu_res = Bits(WIDTH bits)

  alu_res := io.dec2alu.ra_value
  for (func_ele <- reslist_all) {
    for ((key, tup) <- func_ele) {
      if (tup._2 != null) {
        when(key) {
          alu_res := tup._2.asBits
        }
      }
    }
  }


  val branch_taken_s = Bool()
  branch_taken_s := False
  when(io.dec2alu.is_break | io.dec2alu.is_irq | io.dec2alu.is_mret) {
    branch_taken_s := True
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_JUMP) {
    branch_taken_s := True
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_EQ) {
    branch_taken_s := adder_res_zero_flag
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_NE) {
    branch_taken_s := ~adder_res_zero_flag
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_LT) {
    branch_taken_s := adder_res_neg_flag ^ adder_res_ov_flag
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_GE) {
    branch_taken_s := adder_res_neg_flag === adder_res_ov_flag
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_LTU) {
    branch_taken_s := ~adder_cout
  }.elsewhen(io.dec2alu.branch_type === BRANCH_TYPE.BR_GEU) {
    branch_taken_s := adder_cout
  }

  io.alu2fetch_brInfo.branch_not_taken := io.dec2alu.instr_valid & ~(io.dec2alu.branch_type === BRANCH_TYPE.BR_NONE) & ~io.alu2fetch_brInfo.branch_taken
  io.alu2fetch_brInfo.branch_taken := io.dec2alu.instr_valid & branch_taken_s

  when(branch_taken_s | io.dec2alu.alu_optype === ALUOPs.AUIPC) {
    when(io.dec2alu.is_reg_jump) {
      jump_addr_s := io.dec2alu.ra_value.asUInt
    }.elsewhen(io.dec2alu.is_irq) {
      jump_addr_s := io.csr2alu.mtvec.asUInt
    }.elsewhen(io.dec2alu.is_mret) {
      jump_addr_s := io.csr2alu.mepc.asUInt
    }.otherwise {
      jump_addr_s := io.dec2alu.pc
    }
    io.alu2fetch_brInfo.jump_addr := jump_addr_s + io.dec2alu.imm_value.asUInt
  }.otherwise {
    jump_addr_s := 0
    io.alu2fetch_brInfo.jump_addr := 0
  }

  io.alu_wr_port.wr_en := io.dec2alu.instr_valid & io.dec2alu.instr_ack & (~(io.dec2alu.branch_type === BRANCH_TYPE.BR_NONE) | (io.dec2alu.branch_type === BRANCH_TYPE.BR_JUMP))
  io.alu_wr_port.wr_index := io.dec2alu.rd_index
  io.alu_wr_port.wr_value := io.dec2alu.is_csrrw ? io.dec2alu.rb_value | alu_res

  io.alu_wr_csr.valid := io.dec2alu.instr_valid & io.dec2alu.is_csrrw
  io.alu_wr_csr.addr := io.dec2alu.rd_index.resized
  io.alu_wr_csr.value := io.dec2alu.ra_value

  io.dec2alu.instr_ack := ~mul_cause_stall & ~div_cause_stall & ~io.dec2alu.is_break

}

object alu_inst {
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
      .generate(new alu())
  }.printPruned()
}