package EasonLib.Arithmetic.divider

import EasonLib.DesignCompiler.{DesignCompilerFlow, DesignCompiler_config}
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps


class restoring_div_radix4(SIZEINA: Int, SIZEINB: Int) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val dinA = in SInt (SIZEINA bits)
    val dinB = in SInt (SIZEINB bits)

    val dout_vld = out Bool()
    val quot = out SInt (SIZEINA + 1 bits)
    val remainder = out SInt (SIZEINB bits)

  }
  noIoPrefix()

  val quot_sign_out = Reg(Bool()) init (False)
  val rem_sign_out = Reg(Bool()) init (False)

  when(io.din_vld) {
    quot_sign_out := io.dinA.sign ^ io.dinB.sign
    rem_sign_out := io.dinA.sign
  }
  val dinA_abs = UInt(SIZEINA bits)
  val dinB_abs = UInt(SIZEINB bits)
  dinA_abs := io.dinA.abs
  dinB_abs := io.dinB.abs

  val quotient = Reg(Bits(SIZEINA bits)) init (0)

  val divisior = Reg(Bits(SIZEINB + 2 bits)) init (0)
  //  val divisior_neg = Bits(SIZEINB + 1 bits)
  //  divisior_neg := ((~divisior).asUInt + 1).asBits

  when(io.din_vld) {
    divisior := dinB_abs.asBits.resized
  }

  val PR_DW = SIZEINA + SIZEINB + 1
  val p_remainder = Reg(Bits(PR_DW + 1 bits)) init (0)
  val p_r_shift = p_remainder |<< 2
  val p_r_calpart = Bits(SIZEINB + 2 bits)
  val p_r_minus_d = Bits(SIZEINB + 2 bits)
  val p_r_minus_2d = Bits(SIZEINB + 2 bits)
  val p_r_minus_3d = Bits(SIZEINB + 2 bits)

  val p_r_part_next = Bits(SIZEINB + 2 bits)

  p_r_calpart := p_r_shift(PR_DW downto SIZEINA)
  p_r_minus_d := (p_r_calpart.asSInt - divisior.asSInt).asBits
  p_r_minus_2d := (p_r_calpart.asSInt - (divisior.asSInt |<< 1)).asBits
  p_r_minus_3d := (p_r_minus_d.asSInt - (divisior.asSInt |<< 1)).asBits

  val deside_bits = p_r_minus_d.msb ## p_r_minus_2d.msb ## p_r_minus_3d.msb
  val quot_updata_bits = Bits(2 bits)
  switch(deside_bits) {
    is(0) { //000
      p_r_part_next := p_r_minus_3d
      quot_updata_bits := 3
    }
    is(1) { //001
      p_r_part_next := p_r_minus_2d
      quot_updata_bits := 2
    }
    is(2, 3) { //01x
      p_r_part_next := p_r_minus_d
      quot_updata_bits := 1

    }
    is(4, 5, 6, 7) { //1xx
      p_r_part_next := p_r_calpart
      quot_updata_bits := 0
    }
  }
  val CAL_NUM = SIZEINA / 2
  val CNT_DW = log2Up(CAL_NUM)
  val control_cnt = Reg(UInt(CNT_DW bits)) init (0)
  val doing = Reg(Bool()) init (False)
  val finish_pulse = control_cnt === CAL_NUM - 1
  when(io.din_vld) {
    doing.set()
  }.elsewhen(finish_pulse) {
    doing.clear()
  }
  when(io.din_vld) {
    control_cnt := 0
  }.elsewhen(doing) {
    control_cnt := control_cnt + 1
  }.elsewhen(finish_pulse) {
    control_cnt := 0
  }

  when(io.din_vld) {
    p_remainder := B(0, SIZEINB + 2 bits) ## dinA_abs
  }.elsewhen(doing) {
    p_remainder := p_r_part_next ## p_r_shift(SIZEINA - 1 downto 0)
  }
  var i = 0
  for (i <- 0 to CAL_NUM - 1) {
    when(io.din_vld) {
      quotient(SIZEINA - 1 - 2 * i downto SIZEINA - 1 - 2 * i - 1) := B(0, 2 bits)
    }.elsewhen(doing) {
      when(control_cnt === i) {
        quotient(SIZEINA - 1 - 2 * i downto SIZEINA - 1 - 2 * i - 1) := quot_updata_bits
      }
    }
  }

  io.remainder := Mux(rem_sign_out, ~p_remainder(PR_DW - 2 downto SIZEINA).asSInt + 1, p_remainder(PR_DW - 2 downto SIZEINA).asSInt)
  io.quot := Mux(quot_sign_out, (B(1, 1 bits) ## ((~quotient).asUInt + 1).asBits).asSInt, (B(0, 1 bits) ## quotient).asSInt)
  io.dout_vld := RegNext(finish_pulse) init (False)

}


object restoring_div_radix4_inst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode = Verilog,
      targetDirectory = "rtl"
    ).generate(new restoring_div_radix4(SIZEINA = 28, SIZEINB = 10))


  }
}


object restoring_div_radix4_test {

  import scala.math._

  def main(args: Array[String]): Unit = {
    val A = 10
    val B = 9
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode = Verilog,
      targetDirectory = "rtl"
    ).generate(new restoring_div_radix4(SIZEINA = A, SIZEINB = B))
    var start_time = 0.0
    var end_time = 0.0
    val compiled = SimConfig.withFstWave.allOptimisation.compile(
      //val compiled = SimConfig.allOptimisation.compile(
      rtl = new restoring_div_radix4(SIZEINA = A, SIZEINB = B))
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.io.din_vld #= false

      sleep(100)
      println("test")
      start_time = (simTime() / 1000.0).toFloat
      var a, b, c, d = 0


      val A_bound = pow(2, A - 1).toInt
      val B_bound = pow(2, B - 1).toInt
      var true_quot = 0
      var true_rem = 0
      var signed_bit = 0
      var num_bits1 = 0
      var num_bits2 = 0

      dut.clockDomain.waitSampling()
      val driver_thread = fork {
        for (a <- (-A_bound) to (A_bound - 1)) {
          for (b <- (-B_bound) to (-1)) {
            if (b != 0) {
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
      val monitor_thread = fork {
        for (c <- (-A_bound) to (A_bound - 1)) {
          for (d <- (-B_bound) to (-1)) {
            dut.clockDomain.waitSamplingWhere(dut.io.dout_vld.toBoolean)
            // quot
            signed_bit = (((dut.io.quot.toInt >> A) & 1) * 2 - 1)
            if (signed_bit == -1) { // positive
              num_bits1 = dut.io.quot.toInt & ((1 << A) - 1)
            } else { // negative
              num_bits1 = (((~dut.io.quot.toInt) + 1) & ((1 << A) - 1))
            }
            true_quot = (-1) * signed_bit * num_bits1
            // rem
            signed_bit = (((dut.io.remainder.toInt >> (B - 1)) & 1) * 2 - 1)
            if (signed_bit == -1) { // positive
              num_bits2 = dut.io.remainder.toInt & ((1 << (B - 1)) - 1)
            } else { // negative
              num_bits2 = (((~dut.io.remainder.toInt) + 1) & ((1 << (B - 1)) - 1))
            }
            true_rem = (-1) * signed_bit * num_bits2
            assert((c / d) == true_quot, "Quot data Mismatch")
            assert((c % d) == true_rem, "Reminder data Mismatch")
            println(s"PASSED! Input A = ${c}, B = ${d}. DUT Quot = ${true_quot}; Expect Quot = ${c / d}, DUT Rem = ${true_rem}; Expect Rem = ${c % d}")

          }
        }
      }
      monitor_thread.join()
      sleep(10)
      end_time = (simTime() * 1000.0).toFloat
      println("Start sim time:" + start_time.toString)
      println("End sim time:" + end_time.toString)
      simSuccess()

    }
  }
}


object restoring_div_radix4_syn {
  val dc_config = DesignCompiler_config(process = 28, freq = 100)

  val dc = new DesignCompilerFlow(
    design = new restoring_div(SIZEINA = 28, SIZEINB = 10),
    topModuleName = "restoring_div",
    workspacePath = "/mnt/data/projects/kws/Easonlib/syn/restoring_div",
    DCConfig = dc_config,
    designPath = ""
  ).doit()

  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode = Verilog,
      targetDirectory = "rtl"
    ).generate(new restoring_div(SIZEINA = 28, SIZEINB = 10))
  }

}