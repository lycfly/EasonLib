package EasonLib.mini_riscv.csr

import EasonLib.mini_riscv.CSR_ADDR
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class RV_CSRs(WIDTH: Int) extends Bundle{
  val mtvec = Bits(WIDTH bits)
  val mscratch = Bits(WIDTH bits)
  val mepc = Bits(WIDTH bits)
}
case class fetch2csr_if(ADDRWD: Int) extends Bundle with IMasterSlave {
  val update_mret_addr = Flow(UInt(ADDRWD bits))
  val irq_enter_addr = UInt(ADDRWD bits)
  val mret_addr = UInt(ADDRWD bits)
  override def asMaster(): Unit = {
    in(irq_enter_addr)
    in(mret_addr)
    master(update_mret_addr)
  }
}
case class read_csr_if(WIDTH: Int) extends Bundle with IMasterSlave {
  val addr = UInt(11 bits)
  val value = Bits(WIDTH bits)
  override def asMaster(): Unit = {
    out(addr)
    in(value)
  }
}
case class write_csr_if(WIDTH: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr = UInt(11 bits)
  val value = Bits(WIDTH bits)

  override def asMaster(): Unit = {
    out(valid)
    out(addr)
    out(value)
  }
}
case class csr2alu_if(WIDTH: Int) extends Bundle with IMasterSlave {
  val mtvec = Bits(WIDTH bits)
  val mepc = Bits(WIDTH bits)
  override def asMaster(): Unit = {
    out(mtvec)
    out(mepc)
  }
}
class csr(ADDRWD:Int = 32, CSRWIDTH:Int = 32) extends Component {
  val io = new Bundle {
    val ex_wr_csr = slave(write_csr_if(CSRWIDTH))
    val id_rd_csr = slave(read_csr_if(CSRWIDTH))
    val fetch2csr = slave(fetch2csr_if(ADDRWD))
    val csr2alu = master(csr2alu_if(CSRWIDTH))
  }
  noIoPrefix()
  val csr = RV_CSRs(CSRWIDTH)
  csr.flatten.foreach(csr => {csr.setAsReg(); csr.init(csr.getZero)})  //to reg

  // mtvec
  when(io.ex_wr_csr.valid && io.ex_wr_csr.addr === CSR_ADDR.MTVEC){
    csr.mtvec := io.ex_wr_csr.value
  }
  // mscratch
  when(io.ex_wr_csr.valid && io.ex_wr_csr.addr === CSR_ADDR.MSCRATCH) {
    csr.mscratch := io.ex_wr_csr.value
  }
  // mepc
  when(io.ex_wr_csr.valid && io.ex_wr_csr.addr === CSR_ADDR.MEPC) {
    csr.mepc := io.ex_wr_csr.value
  }.elsewhen(io.fetch2csr.update_mret_addr.valid){
    csr.mepc := io.fetch2csr.update_mret_addr.payload.asBits
  }


  switch(io.id_rd_csr.addr){
    is(CSR_ADDR.MTVEC) {io.id_rd_csr.value := csr.mtvec}
    is(CSR_ADDR.MSCRATCH) {io.id_rd_csr.value := csr.mscratch}
    is(CSR_ADDR.MEPC) {io.id_rd_csr.value := csr.mepc}
    default{io.id_rd_csr.value := 0}
  }

  io.fetch2csr.irq_enter_addr := csr.mtvec.asUInt
  io.fetch2csr.mret_addr := csr.mepc.asUInt

  io.csr2alu.mtvec := csr.mtvec
  io.csr2alu.mepc := csr.mepc
}

object mini_riscv_csr_inst {
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
      .generate(new csr())
  }.printPruned()
}