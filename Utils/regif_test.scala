package EasonLib.Utils

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.regif.{Apb3BusInterface, HtmlGenerator}

import scala.util.Random
import scala.language.postfixOps

class regif_test() extends Component {
  val io = new Bundle {
   val apb = slave(Apb3(Apb3Config(16, 32)))
  }
  noIoPrefix()

  val busif = Apb3BusInterface(io.apb, (0x0000, 100 Byte))
  val M_REG0 = busif.newReg(doc = "REG0")
  val M_REG1 = busif.newReg(doc = "REG1")
  val M_REG2 = busif.newReg(doc = "REG2")

  val M_REGn = busif.newRegAt(address = 0x40, doc = "REGn")
  val M_REGn1 = busif.newReg(doc = "REGn1")

  busif.accept(HtmlGenerator("regif", "AP"))
  // busif.accept(CHeaderGenerator("header", "AP"))
  // busif.accept(JsonGenerator("regif"))
  // busif.accept(RalfGenerator("regbank"))
  // busif.accept(SystemRdlGenerator("regif", "AP"))

}

object regif_test_inst {
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
      .generate(new regif_test())
  }.printPruned()
}