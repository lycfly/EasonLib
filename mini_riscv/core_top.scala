package EasonLib.mini_riscv

import EasonLib.mini_riscv.fetch.{fetch, fetch2iram_if}
import EasonLib.mini_riscv.lsu.lsu2mem_if
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps

class core_top(implicit conf: Config) extends Component {
  val io = new Bundle {
//    val fetch2iram = master(fetch2iram_if(conf.ADDRWD, conf.FETCH_WIDTH))
//    val lsu_slot0 = master(lsu2mem_if(conf.XLEN, conf.ADDRWD))
//    val lsu_slot1 = master(lsu2mem_if(conf.XLEN, conf.ADDRWD))
  }
  noIoPrefix()

  println(conf.XLEN)
//  val fetch_unit = new fetch()


}

object core_top_inst {
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
      .generate({
        implicit val config = new Config()
        new core_top()
      })
  }.printPruned()
}