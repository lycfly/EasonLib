package EasonLib.mini_riscv.fetch

import EasonLib.mini_riscv.alu.branch_info
import EasonLib.mini_riscv.csr.fetch2csr_if
import EasonLib.mini_riscv.decode.pre_decode
import EasonLib.mini_riscv.lsu.lsu2scb_if
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps

case class fetch2iram_if(ADDRWD: Int, FETCH_WIDTH: Int) extends Bundle with IMasterSlave {
  val rd_en = Bool()
  val rd_addr = UInt(ADDRWD bits)
  val rd_ready = Bool()
  val rd_grant = Bool()
  val rdata = Bits(FETCH_WIDTH bits)

  override def asMaster(): Unit = {
    in(rdata)
    in(rd_ready)
    in(rd_grant)
    out(rd_en)
    out(rd_addr)
  }
}

case class fetch2dec_if(WIDTH: Int = 32, ADDRWD: Int = 32, ISSUE_WIDTH: Int = 3) extends Bundle with IMasterSlave {
  //  val valid = Bool()
  //  val ready = Bool()
  val instr_slot = Vec(Stream(Bits(WIDTH bits)), ISSUE_WIDTH)
  val pc = UInt(ADDRWD bits)
  val instr_tag = Vec(UInt(2 bits), ISSUE_WIDTH)

  override def asMaster(): Unit = {
    out(pc)
    for (i <- 0 until ISSUE_WIDTH) {
      master(instr_slot(i))
      out(instr_tag(i))
    }
    //    out(valid)
    //    in(ready)
  }
}

class fetch(WIDTH: Int = 32, ADDRWD: Int = 32, FETCH_WIDTH: Int = 64, REGNUM: Int = 32,
            INSTR16_NUM: Int = 8, FETCH16GROUP: Int = 4, ISSUE_WIDTH: Int = 3
           ) extends Component {
  val io = new Bundle {
    val irq_in = in Bool()
    val fetch2iram = master(fetch2iram_if(ADDRWD, FETCH_WIDTH))
    val alu2fetch_brInfo = slave(branch_info(ADDRWD))
    val fetch2dec = master(fetch2dec_if(WIDTH, ADDRWD, ISSUE_WIDTH))
    val fetch2csr = master(fetch2csr_if(ADDRWD))
    val lsu2scb_slot1 = slave(lsu2scb_if(REGNUM))
    val lsu2scb_slot2 = slave(lsu2scb_if(REGNUM))

  }
  noIoPrefix()
  val NUM_LOG2 = log2Up(INSTR16_NUM)
  val FETCH_WIDTH_BYTE = FETCH_WIDTH/8
  val lsu_scoreboard = Bits(REGNUM bits)
  val alu_scoreboard_rd = Reg(UInt(log2Up(REGNUM) bits)) init (0)
  val pc_r = Reg(UInt(ADDRWD bits)) init (0)
  val slot_enable = Vec(Bool(), ISSUE_WIDTH)
  val block_if = Reg(Bool()) init (False)
  val irq_block = Reg(Bool()) init (False)
  val dispatch_num = UInt(NUM_LOG2 bits)
  val instr_issue_valid = Vec(Bool(),ISSUE_WIDTH)
  val flush_fetch_buff = Bool()

  val fetchbuffer = new fetch_buffer(WIDTH, INSTR16_NUM, FETCH16GROUP, ISSUE_WIDTH)
  val pre_decoder = for (i <- 0 until ISSUE_WIDTH) yield new pre_decode(WIDTH, REGNUM)
  fetchbuffer.io.ifbuff_flush := flush_fetch_buff
//  fetchbuffer.io.ifbuff_write.valid := io.fetch2iram.rd_ready & io.fetch2iram.rd_grant
//  fetchbuffer.io.ifbuff_write.payload := io.fetch2iram.rdata
  fetchbuffer.io.dispatch_num := dispatch_num
  fetchbuffer.io.ifbuff_read_valid := instr_issue_valid(0)

  io.fetch2iram.rd_en := fetchbuffer.io.ifbuff_ready
  val addr_inc_flag = io.fetch2iram.rd_en & io.fetch2iram.rd_grant
  val addr_inc_value =  U(FETCH_WIDTH_BYTE, log2Up(FETCH_WIDTH_BYTE)+1 bits) - io.fetch2iram.rd_addr(2 downto 0)
  val fetch_addr_bound_distance = Reg(UInt(5 bits)) init(0)
  when(flush_fetch_buff){
    fetch_addr_bound_distance := 0
  }.elsewhen(addr_inc_flag | instr_issue_valid(0)){
    when(addr_inc_flag){
      fetch_addr_bound_distance := fetch_addr_bound_distance + addr_inc_value.resized
    }
    when(instr_issue_valid(0)){
      fetch_addr_bound_distance := fetch_addr_bound_distance - (dispatch_num ## B"1'b0").asUInt
    }
  }
  io.fetch2iram.rd_addr := pc_r + fetch_addr_bound_distance
// unalign handle
  val unalign_addr_offset = Reg(Bits(log2Up(FETCH_WIDTH_BYTE)-1 bits)) init(0)
  val unalign_offset_catched = Reg(Bits(log2Up(FETCH_WIDTH_BYTE)-1 bits)) init(0)
  when(flush_fetch_buff){
    unalign_addr_offset := 0
  }.elsewhen(addr_inc_flag){
    unalign_addr_offset := (io.fetch2iram.rdata(log2Up(FETCH_WIDTH_BYTE)-1 downto 1))
  }
  when(flush_fetch_buff){
    unalign_offset_catched := 0
  }.elsewhen(io.fetch2iram.rd_ready & (unalign_addr_offset =/= 0)){
    unalign_offset_catched := unalign_addr_offset
  }
  val offset = unalign_addr_offset | unalign_offset_catched
  val unalign_buffer = Reg(Bits(FETCH_WIDTH-16 bits)) init(0)
  when(offset =/= 0 && io.fetch2iram.rd_ready){
    for(i <- 1 until FETCH16GROUP){
      when(offset === i){
        unalign_buffer := B(0, (i-1)*16 bits) ## io.fetch2iram.rdata(FETCH_WIDTH-1 downto 16*i)
      }
    }
  }
  val aligned_instr = Bits(FETCH_WIDTH bits)
  aligned_instr := io.fetch2iram.rdata
  for(i <- 1 until FETCH16GROUP){
    when(offset === i){
      aligned_instr := io.fetch2iram.rdata(16*(i)-1 downto 0) ## unalign_buffer(FETCH_WIDTH-1-16*i downto 0)
    }
  }
  val align_instr_rdy = io.fetch2iram.rd_ready & (unalign_addr_offset === 0)
  val cache_buff = Reg(Bits(FETCH_WIDTH bits)) init(0)
  val cache_buff_valid = Reg(Bool()) init(False)
  val cache_buff_wr_en = align_instr_rdy && (fetchbuffer.io.ifbuffer_full & (dispatch_num < 4))
  val cache_buff_rd_en = cache_buff_valid && (~fetchbuffer.io.ifbuffer_full | (dispatch_num >= 4))

  when(cache_buff_wr_en){
    cache_buff := aligned_instr
  }
  when(flush_fetch_buff){
    cache_buff_valid := False
  }.elsewhen(cache_buff_wr_en | cache_buff_rd_en){
    cache_buff_valid := cache_buff_wr_en
  }

  fetchbuffer.io.ifbuff_write.valid := (align_instr_rdy && (~fetchbuffer.io.ifbuffer_full | (dispatch_num >= 4))) | cache_buff_rd_en
  fetchbuffer.io.ifbuff_write.payload := align_instr_rdy ? aligned_instr | cache_buff

// pre decode
  val instr_hazard_check_fail = Vec(Bool(), ISSUE_WIDTH)
  val instr_slot_valid = Vec(Vec(Bool(), ISSUE_WIDTH), ISSUE_WIDTH)

  for (i <- 0 until ISSUE_WIDTH) {
    slot_enable(i) := ~io.fetch2dec.instr_slot(i).valid || io.fetch2dec.instr_slot(i).ready // stream

    pre_decoder(i).io.instr_in := fetchbuffer.io.instr_candidates(i)
    pre_decoder(i).io.instr_type := fetchbuffer.io.instr_length_bit(i)

  }

  val instr0_rs1 = pre_decoder(0).io.rv_predec.rs1
  val instr0_rs2 = pre_decoder(0).io.rv_predec.rs2
  val instr0_rd = pre_decoder(0).io.rv_predec.rd
  val instr1_rs1 = pre_decoder(1).io.rv_predec.rs1
  val instr1_rs2 = pre_decoder(1).io.rv_predec.rs2
  val instr1_rd = pre_decoder(1).io.rv_predec.rd
  val instr2_rs1 = pre_decoder(2).io.rv_predec.rs1
  val instr2_rs2 = pre_decoder(2).io.rv_predec.rs2
  val instr2_rd = pre_decoder(2).io.rv_predec.rd
  val predec = Array(pre_decoder(0).io.rv_predec, pre_decoder(1).io.rv_predec, pre_decoder(2).io.rv_predec)

  when((predec(0).is_branch | predec(0).is_j) & instr_slot_valid(0)(0)) {
    block_if := True
  }.elsewhen(io.alu2fetch_brInfo.branch_taken | io.alu2fetch_brInfo.branch_not_taken) {
    block_if := False
  }
  when(io.irq_in & ~block_if) {
    irq_block := True
  }.elsewhen(predec(0).is_mret) {
    irq_block := False
  }
  // pc
  when(flush_fetch_buff) {
    pc_r := io.alu2fetch_brInfo.branch_taken ? io.alu2fetch_brInfo.jump_addr | (predec(0).is_mret ? io.fetch2csr.mret_addr | io.fetch2csr.irq_enter_addr)
  }.elsewhen(instr_issue_valid(0)) {
    pc_r := pc_r + (dispatch_num ## B"1'b0").asUInt
  }
  io.fetch2dec.pc := pc_r
  // mret
  val flush_fetch_buff_cond = ArrayBuffer[Bool]()
  flush_fetch_buff_cond += io.alu2fetch_brInfo.branch_taken
  flush_fetch_buff_cond += (predec(0).is_mret | predec(0).is_break) & (~block_if) & fetchbuffer.io.instr_valid_bit(0)
  flush_fetch_buff_cond += io.irq_in & (~block_if) & (~irq_block)

  flush_fetch_buff := flush_fetch_buff_cond.reduce(_ | _)
  io.fetch2csr.update_mret_addr.valid := (predec(0).is_break & fetchbuffer.io.instr_valid_bit(0)) | (io.irq_in & (~block_if) & (~irq_block))
  val mret_adder_offset = UInt(3 bits)
  mret_adder_offset := 0
  when(predec(0).is_break & fetchbuffer.io.instr_valid_bit(0) & fetchbuffer.io.instr_length_bit(0)){
    mret_adder_offset := 4
  }.elsewhen(predec(0).is_break & fetchbuffer.io.instr_valid_bit(0) & ~fetchbuffer.io.instr_length_bit(0)){
    mret_adder_offset := 2
  }
  io.fetch2csr.update_mret_addr.payload := pc_r + mret_adder_offset


  // issue instr0
  instr_hazard_check_fail(0) :=
    (instr0_rs1 =/= 0 & (lsu_scoreboard(instr0_rs1) || (instr0_rs1 === alu_scoreboard_rd))) || //RAW
      (instr0_rs2 =/= 0 & (lsu_scoreboard(instr0_rs2) || (instr0_rs2 === alu_scoreboard_rd))) || //RAW
      (instr0_rd =/= 0 & (lsu_scoreboard(instr0_rd) || (instr0_rd === alu_scoreboard_rd))) //WAW

  val instr0_block_if = predec(0).is_j | predec(0).is_branch | predec(0).is_auipc
  instr_slot_valid(0)(0).setName(s"instr0_slot0_valid") := fetchbuffer.io.instr_valid_bit(0) &
    (predec(0).is_alu | predec(0).is_j | predec(0).is_branch | predec(0).is_auipc) &
    slot_enable(0) & ~instr_hazard_check_fail(0) & ~block_if & (irq_block | ~io.irq_in)

  instr_slot_valid(0)(1).setName(s"instr0_slot1_valid") := fetchbuffer.io.instr_valid_bit(0) & predec(0).is_ld & slot_enable(1) &
    ~instr_hazard_check_fail(0) & ~block_if & (irq_block | ~io.irq_in)
  instr_slot_valid(0)(2).setName(s"instr0_slot2_valid") := fetchbuffer.io.instr_valid_bit(0) & ((predec(0).is_ld & ~slot_enable(1)) | predec(0).is_st) &
    ~instr_hazard_check_fail(0) & ~block_if & (irq_block | ~io.irq_in)

  instr_issue_valid(0) := instr_slot_valid(0).reduce(_ | _)

  // issue instr1
  instr_hazard_check_fail(1) :=
    (instr1_rs1 =/= 0 & (lsu_scoreboard(instr1_rs1) || (instr1_rs1 === alu_scoreboard_rd) || (instr1_rs1 === instr0_rd))) || //RAW
      (instr1_rs2 =/= 0 & (lsu_scoreboard(instr1_rs2) || (instr1_rs2 === alu_scoreboard_rd) || (instr1_rs2 === instr0_rd))) || //RAW
      (instr1_rd =/= 0 & (lsu_scoreboard(instr1_rd) || (instr1_rd === alu_scoreboard_rd) || (instr1_rd === instr0_rd))) //WAW

  instr_slot_valid(1)(0).setName(s"instr1_slot0_valid") := (instr_issue_valid(0) & fetchbuffer.io.instr_valid_bit(1)) &
    (predec(1).is_alu & ~predec(0).is_alu & ~instr0_block_if) & slot_enable(0) & ~instr_hazard_check_fail(1)
  instr_slot_valid(1)(1).setName(s"instr1_slot1_valid") := (instr_issue_valid(0) & fetchbuffer.io.instr_valid_bit(1)) &
    (predec(1).is_ld & ~predec(0).is_ld & ~instr0_block_if) & slot_enable(1) & ~instr_hazard_check_fail(1)
  instr_slot_valid(1)(2).setName(s"instr1_slot2_valid") := (instr_issue_valid(0) & fetchbuffer.io.instr_valid_bit(1)) &
    ((~slot_enable(1) | predec(0).is_ld) | predec(1).is_st) & ~instr_hazard_check_fail(1)

  instr_issue_valid(1) := instr_slot_valid(1).reduce(_ | _)
  // issue instr2
  instr_hazard_check_fail(2) :=
    (instr2_rs1 =/= 0 & (lsu_scoreboard(instr2_rs1) || (instr2_rs1 === alu_scoreboard_rd) || (instr2_rs1 === instr0_rd) || (instr2_rs1 === instr1_rd))) || //RAW
      (instr2_rs2 =/= 0 & (lsu_scoreboard(instr2_rs2) || (instr2_rs2 === alu_scoreboard_rd) || (instr2_rs2 === instr0_rd) || (instr2_rs2 === instr1_rd))) || //RAW
      (instr2_rd =/= 0 & (lsu_scoreboard(instr2_rd) || (instr2_rd === alu_scoreboard_rd) || (instr2_rd === instr0_rd) || (instr2_rd === instr1_rd))) //WAW

  instr_slot_valid(2)(0).setName(s"instr2_slot0_valid") := (instr_issue_valid(1) & fetchbuffer.io.instr_valid_bit(2)) &
    (predec(2).is_alu & ~predec(1).is_alu & ~predec(0).is_alu) & slot_enable(0) & ~instr_hazard_check_fail(2)
  instr_slot_valid(2)(1).setName(s"instr2_slot1_valid") := (instr_issue_valid(1) & fetchbuffer.io.instr_valid_bit(2)) &
    (predec(2).is_ld & ~predec(1).is_ld & ~predec(0).is_ld) & slot_enable(1) & ~instr_hazard_check_fail(1)
  instr_slot_valid(2)(2).setName(s"instr2_slot2_valid") := (instr_issue_valid(1) & fetchbuffer.io.instr_valid_bit(2)) &
    (predec(2).is_ld & (~slot_enable(1) | predec(1).is_ld | predec(0).is_ld) | predec(2).is_st) &
    (slot_enable(2) & (~instr_slot_valid(1)(2) & ~instr_slot_valid(0)(2))) & ~instr_hazard_check_fail(2)

  instr_issue_valid(2) := instr_slot_valid(2).reduce(_ | _)

  dispatch_num := (((False ## instr_issue_valid(0)).asUInt |<< fetchbuffer.io.instr_length_bit(0).asUInt) +
    ((False ## instr_issue_valid(1)).asUInt |<< fetchbuffer.io.instr_length_bit(1).asUInt) +
    ((False ## instr_issue_valid(2)).asUInt |<< fetchbuffer.io.instr_length_bit(2).asUInt)).resized


  for (i <- 0 until ISSUE_WIDTH) {
    when(instr_slot_valid(i)(0) & (predec(i).is_div_mul)) {
      alu_scoreboard_rd := predec(i).rd
    }
  }
  when(io.fetch2dec.instr_slot(0).ready) {
    alu_scoreboard_rd := 0
  }


  for (i <- 0 until ISSUE_WIDTH) {
    val slot_set = ArrayBuffer[Bool]()
    io.fetch2dec.instr_slot(i).payload.setAsReg() init(0)
    io.fetch2dec.instr_slot(i).valid.setAsReg() init(False)
//    io.fetch2dec.instr_slot(i).payload := 0
    for (j <- 0 until ISSUE_WIDTH) {
      slot_set += instr_slot_valid(j)(i)
      when(instr_slot_valid(j)(i)) {
        io.fetch2dec.instr_slot(i).payload := fetchbuffer.io.instr_candidates(j)
      }
    }
    when(slot_set.reduce(_ | _)) {
      io.fetch2dec.instr_slot(i).valid := True
    }.elsewhen(io.fetch2dec.instr_slot(i).ready) {
      io.fetch2dec.instr_slot(i).valid := False
    }
  }

  // lsu scoreboard
  val lsu0_push = Flow(UInt(log2Up(REGNUM) bits))
  val lsu1_push = Flow(UInt(log2Up(REGNUM) bits))

  lsu0_push.valid := False
  lsu0_push.payload := 0
  lsu1_push.valid := False
  lsu1_push.payload := 0
  for (i <- 0 until ISSUE_WIDTH) {
    when(instr_slot_valid(i)(1)) {
      lsu0_push.valid := True
      lsu0_push.payload := predec(i).rd
    }
    when(instr_slot_valid(i)(2)) {
      lsu1_push.valid := True
      lsu1_push.payload := predec(i).rd
    }
  }
  val lsu_scb_r = Reg(Bits(REGNUM bits)) init(0)
  for(i <- 0 until REGNUM){
    when((lsu0_push.valid & (lsu0_push.payload === i)) | (lsu1_push.valid & (lsu1_push.payload === i))){
      lsu_scb_r(i) := True
    }.elsewhen((io.lsu2scb_slot1.pop_scb_valid & io.lsu2scb_slot1.pop_scb_val === i) | (io.lsu2scb_slot2.pop_scb_valid & io.lsu2scb_slot2.pop_scb_val === i)){
      lsu_scb_r(i) := False
    }
    lsu_scoreboard(i) := lsu_scb_r(i) &
      ~(io.lsu2scb_slot1.pop_scb_valid & io.lsu2scb_slot1.pop_scb_val === i) &
      ~(io.lsu2scb_slot2.pop_scb_valid & io.lsu2scb_slot2.pop_scb_val === i)
  }


}

object fetch_inst {
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
      .generate(new fetch())
  }.printPruned()
}