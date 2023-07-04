package EasonLib.mini_riscv

import EasonLib.Utils.memif
import EasonLib.mini_riscv.lsu.lsu2mem_if
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}

import scala.util.Random
import scala.language.postfixOps

object AHBState extends SpinalEnum {
  val IDLE, CONTROL0, CONTROL1 = newElement()
}

object AHBResp {
  def Okey = False
  def Error = True
}

class lsu2ahbmst(implicit conf: Config) extends Component {
  val io = new Bundle {
    val lsu_slot = Vec(slave(lsu2mem_if(conf.XLEN, conf.ADDRWD)), 2)
    val ahb_mst = master(AhbLite3Master(AhbLite3Config(addressWidth = conf.ADDRWD, dataWidth = conf.XLEN)))
    val dmem = Vec(master(memif(ADDRWD = conf.ADDRWD, DATAWD = conf.XLEN, WithSEL = true)), 2)
  }
  noIoPrefix()
  val ahb_mst_ena = Vec(Bool(), 2)
  val mem_select_ena = Vec(Bool(), 2)
  val lsu_req_type = Vec(Bool(), 2)
  for (i <- 0 until 2) {
    ahb_mst_ena(i) := False
    mem_select_ena(i) := False
//    when((io.lsu_slot(i).rd_req | io.lsu_slot(i).wr_req) & ~io.lsu_slot(i).req_ack) {
    when(io.lsu_slot(i).rd_req | io.lsu_slot(i).wr_req) {
      when(io.lsu_slot(i).addr >= conf.DMEM_BASE && io.lsu_slot(i).addr < conf.DMEM_BOUND) {
        mem_select_ena(i) := True
      }.otherwise {
        ahb_mst_ena(i) := True
      }
    }
    lsu_req_type(i) := io.lsu_slot(i).rd_req & ~io.lsu_slot(i).wr_req
  }

  val ahb_mst_req_hold = Vec(Reg(Bool()).init(False), 2)
  val HADDR_hold = UInt(32 bits).setAsReg().init(0)
  val HWRITE_hold = Bool().setAsReg().init(False)
  val HSIZE_hold = Bits(3 bits).setAsReg().init(0)
  val HWDATA_hold = Bits(32 bits).setAsReg().init(0)

  val need_arbite = ahb_mst_ena.reduce(_ & _)
  val lsu_arbited = Vec(Bool(), 2)
  lsu_arbited(0) := io.lsu_slot(0).instr_tag < io.lsu_slot(1).instr_tag
  lsu_arbited(1) := ~lsu_arbited(0)


  val curr_st = AHBState().setAsReg().init(AHBState.IDLE)
  val next_st = AHBState()
  curr_st := next_st

  next_st := curr_st
  switch(curr_st) {
    is(AHBState.IDLE) {
      when(need_arbite) {
        when(lsu_arbited(0)) {
          next_st := AHBState.CONTROL0
        }.elsewhen(lsu_arbited(1)) {
          next_st := AHBState.CONTROL1
        }
      }.elsewhen(ahb_mst_ena(0) | ahb_mst_req_hold(0)) {
        next_st := AHBState.CONTROL0
      }.elsewhen(ahb_mst_ena(1) | ahb_mst_req_hold(1)) {
        next_st := AHBState.CONTROL1
      }
    }
    is(AHBState.CONTROL0) {
      when(io.ahb_mst.HRESP === AHBResp.Okey && io.ahb_mst.HREADY && ahb_mst_ena(0) === False) {
          next_st := AHBState.IDLE
      }
    }
    is(AHBState.CONTROL1) {
      when(io.ahb_mst.HRESP === AHBResp.Okey && io.ahb_mst.HREADY && ahb_mst_ena(1) === False) {
          next_st := AHBState.IDLE
      }
    }
  }

  when(need_arbite){
    ahb_mst_req_hold(0) := ~lsu_arbited(0)
    ahb_mst_req_hold(1) := ~lsu_arbited(1)
  }.elsewhen(curr_st =/= AHBState.IDLE){
    when(ahb_mst_ena(0)){
      ahb_mst_req_hold(0) := True
    }.elsewhen(ahb_mst_ena(1)){
      ahb_mst_req_hold(1) := True
    }
  }.elsewhen(curr_st === AHBState.IDLE){
    ahb_mst_req_hold(0) := False
    ahb_mst_req_hold(1) := False
  }


  def hold_req(i: Int): Unit = {
    HSIZE_hold := io.lsu_slot(i).mem_size.resized
    HADDR_hold := io.lsu_slot(i).addr
    HWDATA_hold := io.lsu_slot(i).wr_data
    HWRITE_hold := io.lsu_slot(i).wr_req & ~io.lsu_slot(i).rd_req
  }

  when(need_arbite){
    when(lsu_arbited(0)){
      hold_req(1)
    }.otherwise{
      hold_req(0)
    }
  }.elsewhen(curr_st =/= AHBState.IDLE && ahb_mst_ena(1)){
    hold_req(1)
  }.elsewhen(curr_st =/= AHBState.IDLE && ahb_mst_ena(0)){
    hold_req(0)
  }


  io.ahb_mst.HWDATA.setAsReg().init(0)
  io.ahb_mst.HBURST := 0
  io.ahb_mst.HPROT := 0
  io.ahb_mst.HSIZE := 2
  io.ahb_mst.HMASTLOCK := False
  when(next_st === AHBState.CONTROL0) {
    io.ahb_mst.HTRANS := AhbLite3.NONSEQ
    when(ahb_mst_req_hold(0) | ahb_mst_req_hold(1)){
      io.ahb_mst.HSIZE := HSIZE_hold
      io.ahb_mst.HADDR := HADDR_hold
      io.ahb_mst.HWDATA := HWDATA_hold
      io.ahb_mst.HWRITE := HWRITE_hold
    }.otherwise{
      io.ahb_mst.HSIZE := io.lsu_slot(0).mem_size.resized
      io.ahb_mst.HADDR := io.lsu_slot(0).addr
      io.ahb_mst.HWDATA := io.lsu_slot(0).wr_data
      io.ahb_mst.HWRITE := io.lsu_slot(0).wr_req & ~io.lsu_slot(0).rd_req
    }
  }.elsewhen(next_st === AHBState.CONTROL1) {
    io.ahb_mst.HTRANS := AhbLite3.NONSEQ
    when(ahb_mst_req_hold(0) | ahb_mst_req_hold(1)) {
      io.ahb_mst.HSIZE := HSIZE_hold
      io.ahb_mst.HADDR := HADDR_hold
      io.ahb_mst.HWDATA := HWDATA_hold
      io.ahb_mst.HWRITE := HWRITE_hold
    }.otherwise {
      io.ahb_mst.HSIZE := io.lsu_slot(1).mem_size.resized
      io.ahb_mst.HADDR := io.lsu_slot(1).addr
      io.ahb_mst.HWDATA := io.lsu_slot(1).wr_data
      io.ahb_mst.HWRITE := io.lsu_slot(1).wr_req & ~io.lsu_slot(1).rd_req
    }
  }.otherwise {
    io.ahb_mst.HTRANS := AhbLite3.IDLE
    io.ahb_mst.HADDR := 0
//    io.ahb_mst.HWDATA := 0
    io.ahb_mst.HWRITE := False
  }

  val mem_select_ena_reg = Vec(Bool(), 2)
  val lsu_req_type_reg = Vec(Bool(), 2)
  val ahb_type_reg = Reg(Bool()).init(False)
  mem_select_ena_reg(0) := RegNext(mem_select_ena(0), False)
  mem_select_ena_reg(1) := RegNext(mem_select_ena(1), False)
  lsu_req_type_reg(0) := RegNext(lsu_req_type(0), False)
  lsu_req_type_reg(1) := RegNext(lsu_req_type(1), False)

  val ahb_start_pulse = (io.ahb_mst.HTRANS =/= 0).rise()
  when(ahb_start_pulse){
    ahb_type_reg := io.ahb_mst.HWRITE
  }
  io.lsu_slot(0).req_ack := False
  io.lsu_slot(0).rd_data_rdy := False
  io.lsu_slot(0).rd_data := 0
  when(mem_select_ena_reg(0)){
    io.lsu_slot(0).req_ack := True
    when(lsu_req_type_reg(0)){
      io.lsu_slot(0).rd_data_rdy := True
      io.lsu_slot(0).rd_data := io.dmem(0).rdata
    }
  }.elsewhen(curr_st === AHBState.CONTROL0) {
    when(io.ahb_mst.HRESP === AHBResp.Okey && io.ahb_mst.HREADY) {
      io.lsu_slot(0).req_ack := True
      io.lsu_slot(0).rd_data_rdy := True
      io.lsu_slot(0).rd_data := io.ahb_mst.HRDATA
    }
  }
  io.lsu_slot(1).req_ack := False
  io.lsu_slot(1).rd_data_rdy := False
  io.lsu_slot(1).rd_data := 0
  when(mem_select_ena_reg(1)){
    io.lsu_slot(1).req_ack := True
    when(lsu_req_type_reg(1)) {
      io.lsu_slot(1).rd_data_rdy := True
      io.lsu_slot(1).rd_data := io.dmem(1).rdata
    }
  }.elsewhen(curr_st === AHBState.CONTROL1) {
    when(io.ahb_mst.HRESP === AHBResp.Okey && io.ahb_mst.HREADY) {
      io.lsu_slot(1).req_ack := True
      when(~ahb_type_reg){
        io.lsu_slot(1).rd_data_rdy := True
        io.lsu_slot(1).rd_data := io.ahb_mst.HRDATA
      }
    }
  }

  for(i <- 0 until 2){
    io.dmem(i).ena := False
    io.dmem(i).wena := False
    io.dmem(i).wdata := 0
    io.dmem(i).addr := 0
    io.dmem(i).wsel.clearAll()
    when(mem_select_ena(i)) {
      io.dmem(i).ena := True
      io.dmem(i).wena := io.lsu_slot(i).wr_req & ~io.lsu_slot(i).rd_req
      io.dmem(i).addr := io.lsu_slot(i).addr
      io.dmem(i).wdata := io.lsu_slot(i).wr_data
      io.dmem(i).wsel := io.lsu_slot(i).byte_sel
    }
  }


}

object lsu2ahbmst_inst {
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
      .generate(new lsu2ahbmst()(new Config()))
  }.printPruned()
}