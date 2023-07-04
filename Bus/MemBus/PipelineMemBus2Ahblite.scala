package EasonLib.Bus.MemBus

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}

import scala.util.Random
import scala.language.postfixOps

object AhbStateEnum extends SpinalEnum {
  val IDLE,CONTROL = newElement()
}

class PipelineMemBus2Ahblite(ahb3Config: AhbLite3Config, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig) extends Component{
  assert(ahb3Config.dataWidth == pipelinedMemoryBusConfig.dataWidth)

  val io = new Bundle {
    val pipelinedMemoryBus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
    val ahb = master(AhbLite3Master(ahb3Config))
  }
  noIoPrefix()
  val curr_state, next_state = AhbStateEnum()
  curr_state.setAsReg().init(AhbStateEnum.IDLE)
  curr_state := next_state
  val ahb_trans_flying = Reg(Bool(),init = False)
  val ahb_handshake_success = ~io.ahb.HRESP & io.ahb.HREADY
  val ahb_trans_finish_one =  ahb_handshake_success & (curr_state === AhbStateEnum.CONTROL)
  
  when(next_state =/= AhbStateEnum.IDLE && io.pipelinedMemoryBus.cmd.valid){
    ahb_trans_flying := io.pipelinedMemoryBus.cmd.valid
  }.elsewhen(ahb_trans_finish_one && next_state =/= AhbStateEnum.CONTROL){
    ahb_trans_flying := False
  }

  next_state := AhbStateEnum.IDLE
  when(io.pipelinedMemoryBus.cmd.valid | ahb_trans_flying){
    next_state := curr_state
    switch(curr_state){
      is(AhbStateEnum.IDLE){
        when(io.pipelinedMemoryBus.cmd.valid){
          next_state := AhbStateEnum.CONTROL
        }
      }
      is(AhbStateEnum.CONTROL){
        when(~io.pipelinedMemoryBus.cmd.valid & ahb_handshake_success){
          next_state := AhbStateEnum.IDLE
        }
      }
    }
  }

  val offset_shift = io.pipelinedMemoryBus.cmd.payload.address(1 downto 0) << 3
  val offset_shift_r = RegNextWhen(offset_shift, io.pipelinedMemoryBus.cmd.valid)

  val wdata_buf = Reg(Bits(pipelinedMemoryBusConfig.dataWidth bits)).init(0)
  when(io.pipelinedMemoryBus.cmd.fire & io.pipelinedMemoryBus.cmd.payload.write){
    wdata_buf := io.pipelinedMemoryBus.cmd.payload.data |<< offset_shift
  }

  val maskWidth = io.pipelinedMemoryBus.cmd.payload.mask.asBits.getWidth
  val write_mask = Vec(Bool(),maskWidth)
  val hsize = UInt(io.ahb.HSIZE.getWidth bits)
  for(i <- 0 until maskWidth){
    write_mask(i) := io.pipelinedMemoryBus.cmd.payload.mask(i)
  }
  hsize := write_mask.sCount(True)

  io.ahb.HTRANS := io.pipelinedMemoryBus.cmd.valid ? AhbLite3.NONSEQ | AhbLite3.IDLE
  io.ahb.HADDR := (io.pipelinedMemoryBus.cmd.valid | ahb_trans_flying) ? io.pipelinedMemoryBus.cmd.payload.address | 0
  io.ahb.HWRITE := (io.pipelinedMemoryBus.cmd.valid | ahb_trans_flying) ? io.pipelinedMemoryBus.cmd.payload.write | False
  io.ahb.HSIZE := hsize.asBits
  io.ahb.HBURST := 0
  io.ahb.HPROT := 1
  io.ahb.HWDATA := wdata_buf
  io.ahb.HMASTLOCK := False

  io.pipelinedMemoryBus.cmd.ready := (ahb_trans_flying & ahb_trans_finish_one) || // just finish one trans
    (curr_state =/= AhbStateEnum.IDLE) ||  // no trans
    (next_state =/= AhbStateEnum.IDLE && io.ahb.HREADY && io.pipelinedMemoryBus.cmd.valid) // one trans arrive

  io.pipelinedMemoryBus.rsp.payload.data := io.ahb.HRDATA |>> offset_shift_r
  io.pipelinedMemoryBus.rsp.valid := ahb_trans_finish_one
}

object PipelineMemBus2Ahblite_inst {
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
      targetDirectory = "rtl_gen")
      .addStandardMemBlackboxing(blackboxAll)
      .generate(new PipelineMemBus2Ahblite(AhbLite3Config(32,32), PipelinedMemoryBusConfig(32,32)))
  }.printPruned()
}