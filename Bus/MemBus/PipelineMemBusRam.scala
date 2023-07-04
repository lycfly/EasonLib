package EasonLib.Bus.MemBus

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}

class PipelineMemBusRam(addressWidth : Int, dataWidth : Int, byteCount: BigInt) extends Component {
  val io = new Bundle {
    val pipeMemBus = slave(PipelinedMemoryBus(PipelinedMemoryBusConfig(addressWidth,dataWidth)))
  }
  noIoPrefix()
  val bytePerWord = dataWidth / 8
  val wordCount = byteCount / bytePerWord
  val ram = Mem(Bits(dataWidth bits), wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(bytePerWord) - 1 downto log2Up(bytePerWord)

  io.pipeMemBus.cmd.ready := True

  val read_enable = io.pipeMemBus.cmd.fire & ~io.pipeMemBus.cmd.payload.write
  io.pipeMemBus.rsp.payload.data := ram.readSync(
    address = io.pipeMemBus.cmd.payload.address(wordRange),
    enable = read_enable
  )
  io.pipeMemBus.rsp.valid := RegNext(read_enable, False)

  ram.write(
    enable = io.pipeMemBus.cmd.fire & io.pipeMemBus.cmd.payload.write,
    address = io.pipeMemBus.cmd.payload.address(wordRange),
    mask = io.pipeMemBus.cmd.payload.mask,
    data = io.pipeMemBus.cmd.payload.data
  )
}
class PipelineMemBusRamMultiPort(portCount : Int, addressWidth : Int, dataWidth : Int, byteCount: BigInt) extends Component {
  val io = new Bundle {
    val pipeMemBus = Vec(slave(PipelinedMemoryBus(PipelinedMemoryBusConfig(addressWidth,dataWidth))), portCount)
  }
  noIoPrefix()
  val bytePerWord = dataWidth / 8
  val wordCount = byteCount / bytePerWord
  val ram = Mem(Bits(dataWidth bits), wordCount.toInt)
  val wordRange = log2Up(wordCount) + log2Up(bytePerWord) - 1 downto log2Up(bytePerWord)

  for(i <- 0 until portCount){
    io.pipeMemBus(i).cmd.ready := True

    val read_enable = io.pipeMemBus(i).cmd.fire & ~io.pipeMemBus(i).cmd.payload.write
    io.pipeMemBus(i).rsp.payload.data := ram.readSync(
      address = io.pipeMemBus(i).cmd.payload.address(wordRange),
      enable = read_enable
    )
    io.pipeMemBus(i).rsp.valid := RegNext(read_enable, False)

    ram.write(
      enable = io.pipeMemBus(i).cmd.fire & io.pipeMemBus(i).cmd.payload.write,
      address = io.pipeMemBus(i).cmd.payload.address(wordRange),
      mask = io.pipeMemBus(i).cmd.payload.mask,
      data = io.pipeMemBus(i).cmd.payload.data
    )
  }

}
object PipelineMemBusRam_inst {
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
//      .addStandardMemBlackboxing(blackboxAll)
      .generate(new PipelineMemBusRam(32,32,8 KiB))
  }.printPruned()
}

object PipelineMemBusRammultiport_inst {
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
//      .addStandardMemBlackboxing(blackboxAll)
      .generate(new PipelineMemBusRamMultiPort(2,32,32,8 KiB))
  }.printPruned()
}