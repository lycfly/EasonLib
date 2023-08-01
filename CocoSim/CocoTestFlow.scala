import scala.io.Source
import scala.sys.process._
import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import java.nio.file.Paths
import scala.collection.mutable

class CocoTestFlow[T <: Component](
                                  design: => T,
                                  workspacePath: String
                                ) {

  private def doCmd(cmd: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  private def doCmd(cmd: String, path: String): Unit = { // do cmd at the workSpace
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !
  }

  private def writeFile(fileName: String, content: String) = {
    val tcl = new java.io.FileWriter(Paths.get(workspacePath, fileName).toFile)
    tcl.write(content)
    tcl.flush()
    tcl.close()
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  def doit() = {
    val inputs = design.getAllIo.filter(_.isInput)
    val inputs_string = for(io <- inputs) yield io.getName()
    val sb = new StringBuilder()
    var input_list = inputs_string.addString(sb," ")
    val top_name = design.getClass.getSimpleName.replace("$", "")
    val coco_path = workspacePath+"/rtl"
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
      targetDirectory = coco_path)
    //   .addStandardMemBlackboxing(blackboxAll)
      .generate({design})
    doCmd(s"python genCocoTemplate.py --target_path=${workspacePath} --simulator=verilator --vpath=${coco_path}/* --vtop=${top_name} --inputs ${input_list}" )
  }

}
