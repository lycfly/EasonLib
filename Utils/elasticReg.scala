package EasonLib.Utils

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.util.Random
import scala.language.postfixOps

class elasticReg[T <: Data](gen: T) extends Module
{
  val entries = 2
  val io = new Bundle{
    val enq = slave Stream(gen)
    val deq = master Stream(gen)
    val count = out UInt(log2Up(entries+1) bits)
  }

  private val valid = Vec(RegInit(False), entries)
  private val elts = Vec(Reg(gen), entries)

  for (i <- 0 until entries) {
    def paddedValid(i: Int) = if (i == -1) True else if (i == entries) False else valid(i)

    val wdata = if (i == entries-1) io.enq.payload else Mux(valid(i+1), elts(i+1), io.enq.payload)
    val wen = Mux(io.deq.ready,
      paddedValid(i+1) || io.enq.fire && (Bool(i == 0) || valid(i)),
      io.enq.fire && paddedValid(i-1) && !valid(i))
    when (wen) { elts(i) := wdata }

    valid(i) := Mux(io.deq.ready,
      paddedValid(i+1) || io.enq.fire && (Bool(i == 0)  || valid(i)),
      io.enq.fire && paddedValid(i-1) || valid(i))
  }

  io.enq.ready := !valid(entries-1)
  io.deq.valid := valid(0)
  io.deq.payload := elts.head

  io.count := CountOne(valid)
}

object elasticReg_inst {
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
      .generate(new elasticReg(UInt(8 bits)))
  }.printPruned()
}