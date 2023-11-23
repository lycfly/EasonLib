package EasonLib.Arithmetic

import scala.math.pow
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random
import scala.language.postfixOps

class Sqrt_unsigned(SIZEIN: Int = 32, ITERATE_NUM: Int = 16) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val din = in UInt (SIZEIN bits)

    val dout_vld = out Bool()
    val dout = out UInt (ITERATE_NUM bits)
  }
  noIoPrefix()
  val root = Reg(UInt(ITERATE_NUM bits)) init(0)
  val rem = Reg(UInt(ITERATE_NUM bits)) init(0)
  val cnt = Reg(UInt(log2Up(ITERATE_NUM) bits)) init(0)
  val cnt_ov = cnt === ITERATE_NUM-1
  val work = Reg(Bool()) init(False)
  val datain = Reg(UInt(SIZEIN bits)) init(0)

  when(io.din_vld){
    datain := io.din
  }
  when(io.din_vld){
    work := True
  }.elsewhen(cnt_ov){
    work := False
  }

  when(work){
    when(cnt_ov){
      cnt := 0
    }.otherwise{
      cnt := cnt + 1
    }
  }

  val work_neg = work.fall(initAt = False)
  val if1 = UInt(root.getWidth bits)
  val temp_rem = UInt(rem.getWidth bits)
  temp_rem := (rem |<< 2) | (datain |>> 30).resize(rem.getWidth)
  if1 := (root |<< 2) + 1
  when(work){
    datain := datain |<< 2
    when(temp_rem >= if1){
      root := (root |<< 1) | U(1, root.getWidth bits)
      rem := temp_rem - if1.resized
    }.otherwise{
      root := root |<< 1
      rem := temp_rem
    }
  }.elsewhen(work_neg){
    root := 0
    rem := 0
  }

  io.dout_vld := RegNext(work_neg, False)
  io.dout.setAsReg().init(0)
  when(work_neg){
    io.dout := root
  }

}

object Sqrt_unsigned_inst {
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
      .generate(new Sqrt_unsigned())
  }.printPruned()

  val compiled = SimConfig.withWave.allOptimisation.compile(
    //val compiled = SimConfig.allOptimisation.compile(
    rtl = new Sqrt_unsigned())
  compiled.doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.io.din_vld #= false

    sleep(100)
    println("test")
    val start_time = (simTime() / 1000.0).toFloat
    var a, b, c, d = 0

    val A_bound = pow(2, 24).toInt
//    val A_bound = 32768

    dut.clockDomain.waitSampling()
    val driver_thread = fork {
      for (a <- (0) until  (A_bound )) {
          dut.io.din_vld #= true
          dut.io.din #= a
          dut.clockDomain.waitSampling()
          dut.io.din_vld #= false
          dut.clockDomain.waitSamplingWhere(dut.io.dout_vld.toBoolean)
      }
    }
    val monitor_thread = fork {
      for (c <- (0) until (A_bound )) {
          dut.clockDomain.waitSamplingWhere(dut.io.dout_vld.toBoolean)
          val true_out = (scala.math.sqrt(c.toDouble).toLong)
          assert((true_out) == dut.io.dout.toLong, "data Mismathc")
          println(s"PASSED! Input A = ${c}. DUT Result = ${dut.io.dout.toLong}; Expect Result = ${true_out}")

      }
    }
    monitor_thread.join()
    sleep(10)
    val end_time = (simTime() / 1000.0).toFloat
    println(start_time)
    println(end_time)
    simSuccess()

  }
}