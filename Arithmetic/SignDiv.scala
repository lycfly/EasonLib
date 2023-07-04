package EasonLib.Arithmetic

import EasonLib.DesignCompiler.{DesignCompilerFlow, DesignCompiler_config}
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class SignDiv (SIZEINA: Int, SIZEINB:Int) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val dinA = in SInt (SIZEINA bits)
    val dinB = in SInt (SIZEINB bits)

    val dout_vld = out Bool()
    val quot = out SInt (SIZEINA+1 bits)
    val remainder = out SInt (SIZEINB bits)


  }
  val Result = Reg(SInt(SIZEINA+1 bits)) init(0)
  val dout_vld_reg = RegNext(io.din_vld) init(false)
  when(io.din_vld){
    Result :=   (io.dinA.msb ## io.dinA.asBits).asSInt / io.dinB
  }
  io.quot := Result
  io.dout_vld := dout_vld_reg
  io.remainder:= 0
}

object SignDiv_inst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode=Verilog,
      targetDirectory="rtl"
      ).generate(new SignDiv(SIZEINA = 28, SIZEINB = 10))


  }
}


object SignDiv_test{
  import scala.math._
  def main(args: Array[String]): Unit = {
    val A = 8
    val B = 4
    val testcase_coverage = 0.05
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
        mode=Verilog,
        targetDirectory="rtl"
      ).generate(new SignDiv(SIZEINA = A, SIZEINB = B))
  var start_time = 0.0
  var end_time = 0.0
  val compiled = SimConfig.withFstWave.allOptimisation.compile(
  //val compiled = SimConfig.allOptimisation.compile(
    rtl = new SignDiv(SIZEINA = A, SIZEINB = B))
  compiled.doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.io.din_vld #= false

    sleep(100)
    println("test")
    start_time = (simTime()/1000.0).toFloat
    var a,b,c,d = 0
    val A_bound = pow(2,A-1).toInt
    val B_bound = pow(2,B-1).toInt
    var true_out = 0
    var signed_bit = 0
    var num_bits = 0

    dut.clockDomain.waitSampling()
    val driver_thread = fork{
      for (a <- (-A_bound) to (A_bound-1)){
        for (b <- (-B_bound) to (-1)) {
          if(b != 0){
            dut.io.din_vld #= true
            dut.io.dinA #= a
            dut.io.dinB #= b
            dut.clockDomain.waitSampling()
            dut.io.din_vld #= false
            dut.clockDomain.waitSamplingWhere(dut.io.dout_vld.toBoolean)
          }
        }
      }
    }
    val monitor_thread = fork{
       for (c <- (-A_bound) to (A_bound-1)){
        for (d <- (-B_bound) to (-1)) {
          dut.clockDomain.waitSamplingWhere(dut.io.dout_vld.toBoolean)
          signed_bit = (((dut.io.quot.toInt >> A) & 1)*2-1)
          if(signed_bit == -1){
            num_bits =  dut.io.quot.toInt & ((1<<A)-1)
          }else{
            num_bits =(((~dut.io.quot.toInt)+1) & ((1<<A)-1))
          }
          true_out = (-1) * signed_bit * num_bits
          assert((c/d)==true_out,"data Mismathc")
          
          println(s"PASSED! Input A = ${c}, B = ${d}. DUT Result = ${true_out}; Expect Result = ${c/d}")

        }}
    }
    monitor_thread.join()
    sleep(10)
    end_time = (simTime()*testcase_coverage/1000.0).toFloat
    println(start_time)
    println(end_time)
    simSuccess()

  }
}
}

object SignDiv_syn {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode=Verilog,
      targetDirectory="rtl"
      ).generate(new SignDiv(SIZEINA = 28, SIZEINB = 10))
  }

  val dc_config = DesignCompiler_config(process = 28, freq = 100)
   val dc = new DesignCompilerFlow(
                                     design = new SignDiv(SIZEINA = 28, SIZEINB = 10),
                                     topModuleName = "SignDiv",
                                     workspacePath = "/mnt/data/projects/kws/Easonlib/syn/SignDiv",
                                     DCConfig = dc_config,
                                     designPath = ""
   ).doit()

}
