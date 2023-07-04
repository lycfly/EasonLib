package EasonLib.mini_riscv.regfile

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps

case class regfile_signle_rd_port(WIDTH: Int = 32, REGNUM: Int = 32) extends Bundle with IMasterSlave {
  val REGNUM_LOG2 = log2Up(REGNUM)
  val ra_index = UInt(REGNUM_LOG2 bits)
  val ra_value = Bits(WIDTH bits)
  override def asMaster(): Unit = {
    out(ra_index)
    in(ra_value)
  }
}
case class regfile_dual_rd_port(WIDTH: Int = 32, REGNUM: Int = 32) extends Bundle with IMasterSlave {
  val REGNUM_LOG2 = log2Up(REGNUM)
  val ra_index = UInt(REGNUM_LOG2 bits)
  val ra_value = Bits(WIDTH bits)
  val rb_index = UInt(REGNUM_LOG2 bits)
  val rb_value = Bits(WIDTH bits)
  override def asMaster(): Unit = {
    out(ra_index)
    in(ra_value)
    out(rb_index)
    in(rb_value)
  }
}

case class regfile_wr_port(WIDTH: Int = 32, REGNUM: Int = 32) extends Bundle with IMasterSlave {
  val REGNUM_LOG2 = log2Up(REGNUM)
  val wr_en = Bool()
  val wr_index = UInt(REGNUM_LOG2 bits)
  val wr_value = Bits(WIDTH bits)
  override def asMaster(): Unit = {
    out(wr_en)
    out(wr_index)
    out(wr_value)
  }
}

class regfile(WIDTH: Int = 32, REGNUM: Int = 32) extends Component {
  val io = new Bundle {
    val alu_rd_port = slave(regfile_dual_rd_port(WIDTH,REGNUM))
    val ld0_rd_port = slave(regfile_signle_rd_port(WIDTH,REGNUM))
    val ldst1_rd_port = slave(regfile_dual_rd_port(WIDTH,REGNUM))

    val alu_wr_port = slave(regfile_wr_port(WIDTH,REGNUM))
    val ld0_wr_port = slave(regfile_wr_port(WIDTH,REGNUM))
    val ldst1_wr_port = slave(regfile_wr_port(WIDTH,REGNUM))

  }
  noIoPrefix()
  val reg_r = Vec(Bits(WIDTH bits), REGNUM)
  reg_r(0) := 0
  for(i <- 1 until REGNUM){
    reg_r(i).setAsReg().init(reg_r(i).getZero)
  }
  val alu_wr_en = Bits(REGNUM bits)
  val ld0_wr_en = Bits(REGNUM bits)
  val ldst1_wr_en = Bits(REGNUM bits)
  alu_wr_en(0) := False
  ld0_wr_en(0) := False
  ldst1_wr_en(0) := False
  for(i <- 1 until REGNUM){
    alu_wr_en(i) := io.alu_wr_port.wr_en && (io.alu_wr_port.wr_index === i)
    ld0_wr_en(i) := io.ld0_wr_port.wr_en && (io.ld0_wr_port.wr_index === i)
    ldst1_wr_en(i) := io.ldst1_wr_port.wr_en && (io.ldst1_wr_port.wr_index === i)
    // may have WAW harzard
    when(alu_wr_en(i) ){
      reg_r(i) := io.alu_wr_port.wr_value
    }.elsewhen(ld0_wr_en(i)){
      reg_r(i) := io.ld0_wr_port.wr_value
    }.elsewhen(ldst1_wr_en(i)){
      reg_r(i) := io.ldst1_wr_port.wr_value
    }
  }

  // alu result fwd to alu/ld/ldst
  val ld0_ra_fwd_flag = io.alu_wr_port.wr_en & (io.ld0_rd_port.ra_index === io.alu_wr_port.wr_index) & ~(io.ld0_rd_port.ra_index === 0)
  io.ld0_rd_port.ra_value := Mux(ld0_ra_fwd_flag, io.alu_wr_port.wr_value, reg_r(io.ld0_rd_port.ra_index))

  val ldst_ra_fwd_flag = io.alu_wr_port.wr_en & (io.ldst1_rd_port.ra_index === io.alu_wr_port.wr_index) & ~(io.ldst1_rd_port.ra_index === 0)
  io.ldst1_rd_port.ra_value := Mux(ldst_ra_fwd_flag, io.alu_wr_port.wr_value, reg_r(io.ldst1_rd_port.ra_index))
  val ldst_rb_fwd_flag = io.alu_wr_port.wr_en & (io.ldst1_rd_port.rb_index === io.alu_wr_port.wr_index) & ~(io.ldst1_rd_port.rb_index === 0)
  io.ldst1_rd_port.rb_value := Mux(ldst_rb_fwd_flag, io.alu_wr_port.wr_value, reg_r(io.ldst1_rd_port.rb_index))

  val alu_ra_fwd_flag = io.alu_wr_port.wr_en & (io.alu_rd_port.ra_index === io.alu_wr_port.wr_index) & ~(io.alu_rd_port.ra_index === 0)
  io.alu_rd_port.ra_value := Mux(alu_ra_fwd_flag, io.alu_wr_port.wr_value, reg_r(io.alu_rd_port.ra_index))
  val alu_rb_fwd_flag = io.alu_wr_port.wr_en & (io.alu_rd_port.rb_index === io.alu_wr_port.wr_index) & ~(io.alu_rd_port.rb_index === 0)
  io.alu_rd_port.rb_value := Mux(alu_rb_fwd_flag, io.alu_wr_port.wr_value, reg_r(io.alu_rd_port.rb_index))
}

object regfile_inst {
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
      .generate(new regfile())
  }.printPruned()
}