package EasonLib.Arithmetic


import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps

class Booth2SmallArea (SIZEINA: Int, SIZEINB:Int) extends Component {
  val io = new Bundle {
    val din_vld = in Bool()
    val dinA = in SInt (SIZEINA bits)
    val dinB = in SInt (SIZEINB bits)

    val dout_vld = out Bool()
    val dout = out SInt (SIZEINA + SIZEINB bits)

  }
  val MAX_NUM = SIZEINA - 1
  val MAX_DW = log2Up(MAX_NUM)
  val shiftReg = Reg(Bits(SIZEINA + SIZEINB + 1 bits)) init(0)
  val flag_bits = Bits(2 bits)
  val NegativeB = SInt(SIZEINB bits)
  val sumShiftB = Bits(SIZEINB bits)
  val addMux  = SInt(SIZEINB bits)
  val cal_cnt = Reg(UInt(MAX_DW bits)) init(0)
  val cal_en = Reg(Bool()) init(false)
  flag_bits := shiftReg(1 downto 0)
  NegativeB := -io.dinB
    addMux  := Mux(flag_bits(1) & ~flag_bits(0), NegativeB, io.dinB)
  sumShiftB := (shiftReg(SIZEINA+SIZEINB downto SIZEINA+1).asSInt + addMux).asBits

  when(io.din_vld){
    cal_en := Bool(true)
  }.elsewhen(cal_cnt === MAX_NUM){
    cal_en := Bool(false)
  }
  when(cal_en){
    cal_cnt := cal_cnt + 1
  }.elsewhen(io.din_vld){
    cal_cnt := 0
  }
  when(io.din_vld){
    shiftReg(SIZEINA downto 1) := io.dinA.asBits
  }.elsewhen(cal_en){
    switch(flag_bits){
      is(0,3){
        shiftReg := shiftReg.asSInt.sign ## shiftReg(SIZEINA+SIZEINB downto 1)
      }
      is(1,2){
        shiftReg  := sumShiftB.asSInt.sign ## sumShiftB ## shiftReg(SIZEINA downto 1)  //shift and add
      }
    }
  }
  io.dout_vld := cal_en.fall
  io.dout := shiftReg(SIZEINA+SIZEINB downto 1).asSInt

//  io.dout := io.dinA * io.dinB
}
  object Booth2SmallAreaInst {
    def main(args: Array[String]): Unit = {
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
          clockEdge = RISING,
          resetActiveLevel = LOW),
        mode=Verilog).generate(new Booth2SmallArea(SIZEINA = 8, SIZEINB = 8))


    }
    val compiled = SimConfig.withWave.allOptimisation.compile(
      rtl = new Booth2SmallArea(SIZEINA = 8, SIZEINB = 8))
    compiled.doSim { dut =>
      val N = 8
      val in_buffer1 = Array(101,72,0,-72,-101,-72,0,72)
      val in_buffer2 = Array(97,47,-30,-90,-97,-47,30,90)

      dut.clockDomain.forkStimulus(5)
      dut.io.din_vld #= false

      sleep(100)
      println("test")
      dut.clockDomain.waitSampling()
      dut.io.din_vld #= true
      dut.io.dinA #= -34
      dut.io.dinB #= 22
      dut.clockDomain.waitSampling()
      dut.io.din_vld #= false
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSamplingWhere(dut.io.dout_vld.toBoolean)
      sleep(100)


    }
    // import DesignCompiler._
    // val dc_config = DesignCompiler_config(process = 28, freq = 100)
    // val dc = new DesignCompilerFlow(
    //   design = new Booth2SmallArea(SIZEINA = 8, SIZEINB = 8),
    //   topModuleName = "Booth2SmallArea",
    //   workspacePath = "/mnt/mydata/Easonlib/syn/Booth2SmallArea",
    //   DCConfig = dc_config,
    //   designPath = ""
    // ).doit()
  }