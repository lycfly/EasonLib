package EasonLib.Utils

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3CrossbarFactory, AhbLite3Master}

import scala.util.Random
import scala.language.postfixOps

class ahbcrossbar() extends Component {

  val ahbConfig = AhbLite3Config(addressWidth = 16, dataWidth = 32)

  val io = new Bundle {
    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)), 3)
    val ahbSlaves = Vec(master(AhbLite3(ahbConfig)), 4)
  }
  noIoPrefix()

  val crossbar = AhbLite3CrossbarFactory(ahbConfig)
    .addSlaves(
      io.ahbSlaves(0) -> (0x1000, 0x1000),
      io.ahbSlaves(1) -> (0x3000, 0x1000),
      io.ahbSlaves(2) -> (0x4000, 0x1000),

      io.ahbSlaves(3) -> (0x5000, 0x1000)
    )
    .addConnections(
      io.ahbMasters(0).toAhbLite3() -> List(io.ahbSlaves(0), io.ahbSlaves(1)),
      io.ahbMasters(1).toAhbLite3() -> List(io.ahbSlaves(1), io.ahbSlaves(2), io.ahbSlaves(3)),
      io.ahbMasters(2).toAhbLite3() -> List(io.ahbSlaves(0), io.ahbSlaves(3))
    )
    // ** OPTIONAL **
    //.addGlobalDefaultSlave(io.defaultSalve)
    //.addDefaultSalves(
    //   io.ahbMaster(0) -> io.defaultSlaveM0,
    //   io.ahbMaster(1) -> io.defaultSalveM1
    //)
    .build()

}

object ahbcrossbar_inst {
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
      .generate(new ahbcrossbar())
  }.printPruned()
}