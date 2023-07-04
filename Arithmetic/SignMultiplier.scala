package EasonLib.Arithmetic

import EasonLib.DesignCompiler.{DesignCompilerFlow, DesignCompiler_config}
import spinal.core._

import scala.language.postfixOps

class SignMultiplier (SIZEINA: Int, SIZEINB:Int, withOutReg:Boolean = true) extends Component {
  val io = new Bundle {
    val dinA = in SInt (SIZEINA bits)
    val dinB = in SInt (SIZEINB bits)
    val din_vld = in Bool()

    val dout = out SInt (SIZEINA + SIZEINB bits)
    val dout_vld = out Bool()

  }
  noIoPrefix()

  val Result = SInt(SIZEINB + SIZEINA bits)
  val dout_vld_reg = Bool()

  if(withOutReg){
    Result.setAsReg().init(0)
    dout_vld_reg.setAsReg().init(False)
  }

  Result.clearAll()
  when(io.din_vld){
    Result := io.dinA * io.dinB
  }
  dout_vld_reg.clear()
  when(io.din_vld){
    dout_vld_reg := True
  }.otherwise{
    dout_vld_reg := False
  }
  io.dout := Result
  io.dout_vld := dout_vld_reg
}
object SignMultiplierInst {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      mode=Verilog).generate(new SignMultiplier(SIZEINA = 8, SIZEINB = 8))


  }

//   import DesignCompiler._
//   val dc_config = DesignCompiler_config(process = 28, freq = 100)
//   val dc = new DesignCompilerFlow(
//                                     design = new SignMultiplier(SIZEINA = 16, SIZEINB = 16),
//                                     topModuleName = "SignMultiplier",
//                                     workspacePath = "/mnt/data/projects/kws/Easonlib/syn/SignMultiplier",
//                                     DCConfig = dc_config,
//                                     designPath = ""
//   ).doit()
}

object signmul_inst_syn {
  def main(args: Array[String]): Unit = {

    val dc_config = DesignCompiler_config(process = 180, freq = 20)
    val dc = new DesignCompilerFlow(
      design = new SignMultiplier(SIZEINA = 8, SIZEINB = 8),
      topModuleName = "SignMultiplier",
      workspacePath = "/home/lyc/projects/kws/tiny_vector_core/syn/SignMultiplier",
      DCConfig = dc_config,
      designPath = ""
    ).doit()
  }
}