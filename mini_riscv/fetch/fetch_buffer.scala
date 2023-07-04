package EasonLib.mini_riscv.fetch

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps



class fetch_buffer(WIDTH: Int=32, INSTR16_NUM: Int = 8, FETCH16GROUP: Int = 4, ISSUE_WIDTH: Int = 3) extends Component {
  val NUM_LOG2 = log2Up(INSTR16_NUM)
  val io = new Bundle {
    val ifbuff_flush = in Bool()
    val ifbuff_write = slave(Flow(Bits(16*FETCH16GROUP bits)))  // default fetch depth: 64 bits
    val dispatch_num = in UInt(NUM_LOG2 bits)

    val ifbuff_read_valid = in Bool()
    val instr_candidates = out(Vec(Bits(WIDTH bits), ISSUE_WIDTH))
    val instr_length_bit = out Bits(ISSUE_WIDTH bits)   // 0: 16 bits , 1: 32 bits instr
    val instr_valid_bit = out Bits(ISSUE_WIDTH bits)

    val ifbuff_ready = out Bool()

    val inst16_buffer = out Vec(Bits(16 bits), INSTR16_NUM)
    val wr_ptr = out UInt(NUM_LOG2+1 bits)
    val rd_ptr = out UInt(NUM_LOG2+1 bits)
    val ifbuffer_full = out Bool()
  }
  noIoPrefix()
  io.inst16_buffer.setAsReg().foreach(a => a.init(0))
  io.wr_ptr.setAsReg().init(0)
  io.rd_ptr.setAsReg().init(0)

  val data_remain_cnt = Reg(UInt(NUM_LOG2+1 bits)) init(0)
  io.ifbuffer_full := data_remain_cnt === INSTR16_NUM
//  val ifbuffer_empty = data_remain_cnt === 0
 // io.ifbuff_ready := data_remain_cnt <= (INSTR16_NUM - FETCH16GROUP)     // free space >= fetch group size
  io.ifbuff_ready := data_remain_cnt < INSTR16_NUM     // free space >= fetch group size

  when(io.ifbuff_write.valid){
    for(i <- 0 until FETCH16GROUP){
      io.inst16_buffer(io.wr_ptr(NUM_LOG2-1 downto 0) + i) := io.ifbuff_write.payload(16*(i+1)-1 downto 16*i)
    }
  }
  when(io.ifbuff_flush){
    io.wr_ptr := 0
  }.elsewhen(io.ifbuff_write.valid){
    io.wr_ptr := io.wr_ptr + FETCH16GROUP
  }

  when(io.ifbuff_flush){
    io.rd_ptr := 0
  }.elsewhen(io.ifbuff_read_valid){
    io.rd_ptr := io.rd_ptr + io.dispatch_num
  }

  when(io.ifbuff_flush){
    data_remain_cnt := 0
  }.elsewhen(io.ifbuff_write.valid | io.ifbuff_read_valid){
    data_remain_cnt := data_remain_cnt + (io.ifbuff_write.valid ? U(FETCH16GROUP) | U(0)) - (io.ifbuff_read_valid ?  io.dispatch_num | U(0))
  }

  val compress_bit = Bits(INSTR16_NUM bits)
  val valid_bit = Bits(INSTR16_NUM bits)
  val ptr_diff = io.wr_ptr.msb ^ io.rd_ptr.msb
  val true_wr_ptr = ptr_diff ## io.wr_ptr(NUM_LOG2-1 downto 0)
  val true_rd_ptr = False ## io.rd_ptr(NUM_LOG2-1 downto 0)

  for(i <- 0 until INSTR16_NUM){
    compress_bit(i) := ~(io.inst16_buffer(i)(1) & io.inst16_buffer(i)(0))
    valid_bit(i) := False
    when(true_rd_ptr.asUInt <= i && true_wr_ptr.asUInt > i) {
      valid_bit(i) := True
    }
  }

  val next_instr_ptr = Vec(UInt(NUM_LOG2 bits),ISSUE_WIDTH)
  next_instr_ptr(0) := io.rd_ptr(NUM_LOG2-1 downto 0)
  io.instr_candidates(0) := compress_bit(next_instr_ptr(0)) ? (B(0,16 bits) ## io.inst16_buffer(next_instr_ptr(0))) | (io.inst16_buffer(next_instr_ptr(0) + 1) ## io.inst16_buffer(next_instr_ptr(0)))
  io.instr_length_bit(0) := ~compress_bit(next_instr_ptr(0))
  io.instr_valid_bit(0) := valid_bit(next_instr_ptr(0))

  for(i <- 1 until ISSUE_WIDTH){
    next_instr_ptr(i) := compress_bit(next_instr_ptr(i-1)) ? (next_instr_ptr(i-1) + 1) | (next_instr_ptr(i-1) + 2)
    io.instr_candidates(i) := compress_bit(next_instr_ptr(i)) ? (B(0,16 bits) ## io.inst16_buffer(next_instr_ptr(i))) | (io.inst16_buffer(next_instr_ptr(i) + 1) ## io.inst16_buffer(next_instr_ptr(i)))
    io.instr_length_bit(i) := ~compress_bit(next_instr_ptr(i))
    io.instr_valid_bit(i) := valid_bit(next_instr_ptr(i))
  }

}

object fetch_buffer_inst {
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
      .generate(new fetch_buffer())
  }.printPruned()
}