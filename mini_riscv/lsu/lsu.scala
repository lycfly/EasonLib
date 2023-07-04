package EasonLib.mini_riscv.lsu

import EasonLib.mini_riscv.regfile.regfile_wr_port
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps
case class decode2lsu_if(WIDTH: Int = 32, ADDRWD: Int = 32, REGNUM: Int = 32) extends Bundle with IMasterSlave {
  val instr_valid = Bool()
  val instr_ack = Bool()
  val rd_index = UInt(log2Up(REGNUM) bits)
  val is_load = Bool()
  val is_store = Bool()
  val mem_signed = Bool()
  val mem_size = Bits(2 bits)
  val lsu_wr_data = Bits(WIDTH bits)
  val lsu_addr = UInt(ADDRWD bits)
  val instr_tag = UInt(2 bits)

  override def asMaster(): Unit = {
    out(instr_valid)
    out(rd_index)
    out(is_load)
    out(is_store)
    out(mem_signed)
    out(mem_size)
    out(lsu_wr_data)
    out(lsu_addr)
    out(instr_tag)
    in(instr_ack)
  }
}
case class lsu2mem_if(WIDTH: Int = 32, ADDRWD: Int = 32) extends Bundle with IMasterSlave {
  val rd_req = Bool()
  val wr_req = Bool()
  val wr_data = Bits(WIDTH bits)
  val byte_sel = Bits(WIDTH/8 bits)
  val mem_size = Bits(2 bits)
  val addr = UInt(ADDRWD bits)
  val req_ack = Bool()
  val rd_data_rdy = Bool()
  val rd_data = Bits(WIDTH bits)
  val instr_tag = UInt(2 bits)

  override def asMaster(): Unit = {
    out(rd_req)
    out(wr_req)
    out(byte_sel)
    out(mem_size)
    out(wr_data)
    out(addr)
    out(instr_tag)
    in(rd_data)
    in(rd_data_rdy)
    in(req_ack)
  }
}
case class lsu2scb_if(REGNUM: Int = 32) extends Bundle with IMasterSlave {
  val pop_scb_valid = Bool()
  val pop_scb_val = UInt(log2Up(REGNUM) bits)
  override def asMaster(): Unit = {
    out(pop_scb_valid)
    out(pop_scb_val)
  }
}


class lsu(WIDTH: Int = 32,ADDRWD: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val dec2lsu = slave(decode2lsu_if(WIDTH,ADDRWD,REGNUM))
    val lsu2mem = master(lsu2mem_if(WIDTH,ADDRWD))
    val lsu2rf = master(regfile_wr_port(WIDTH, REGNUM))
    val lsu2scb = master(lsu2scb_if(REGNUM))
  }
  noIoPrefix()

  io.dec2lsu.instr_ack := io.lsu2mem.req_ack
  io.lsu2mem.rd_req := io.dec2lsu.instr_valid & io.dec2lsu.is_load
  io.lsu2mem.wr_req := io.dec2lsu.instr_valid & io.dec2lsu.is_store
  io.lsu2mem.addr := io.dec2lsu.lsu_addr
  io.lsu2mem.wr_data := io.dec2lsu.lsu_wr_data
  io.lsu2mem.instr_tag := io.dec2lsu.instr_tag

  val mem_signed_r = Reg(Bool(), False)
  val mem_size_r = Reg(Bits(2 bits)) init(0)
  val rd_index_r = Reg(UInt(log2Up(REGNUM) bits)) init(0)

  when(io.dec2lsu.is_load & io.lsu2mem.req_ack){
    mem_signed_r := io.dec2lsu.mem_signed
    mem_size_r := io.dec2lsu.mem_size
    rd_index_r := io.dec2lsu.rd_index
  }

  io.lsu2rf.wr_en := io.lsu2mem.rd_data_rdy
  io.lsu2rf.wr_index := rd_index_r

  switch(mem_size_r){
    is(0) {
      io.lsu2rf.wr_value := (mem_signed_r & io.lsu2mem.rd_data.msb).asSInt.resize(24) ## io.lsu2mem.rd_data(7 downto 0)
    }
    is(1) {
      io.lsu2rf.wr_value := (mem_signed_r & io.lsu2mem.rd_data.msb).asSInt.resize(16) ## io.lsu2mem.rd_data(15 downto 0)
    }
    default {
      io.lsu2rf.wr_value := io.lsu2mem.rd_data
    }
  }

  io.lsu2scb.pop_scb_valid := io.lsu2mem.rd_data_rdy
  io.lsu2scb.pop_scb_val := rd_index_r


}

object lsu_inst {
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
      .generate(new lsu())
  }.printPruned()
}