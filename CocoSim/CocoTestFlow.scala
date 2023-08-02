package EasonLib.CocoSim

import com.google.common.collect.Maps
import com.hubspot.jinjava.{Jinjava, JinjavaConfig}

import scala.io.Source
import scala.sys.process._
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.sim.SimConfig

import java.io.{File, FileWriter}
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
    val tcl = new java.io.FileWriter(fileName)
    //Paths.get(workspacePath, fileName).toFile
    tcl.write(content)
    tcl.flush()
    tcl.close()
  }
  private def writeFileNoExist(fileName: String, content: String) = {
    val file = new java.io.File(fileName)
    if(!file.exists()){
      val tcl = new FileWriter(fileName)
      tcl.write(content)
      tcl.flush()
      tcl.close()
    }

  }
  def gentemplate(template_path: String, gen_path: String, context: mutable.Map[String, Any]) = {
    val source = Source.fromFile(template_path)
    val template = source.mkString
    source.close()
    val jinjava = new Jinjava()
    val maps = Maps.newHashMap[String, Any]()
    for ((dict, value) <- context) {
      maps.put(dict, value)
    }
    val fileTemplate = jinjava.render(template, maps)
    val write_file = new File(gen_path)
    if(!write_file.exists()){
      writeFile(gen_path, fileTemplate)
    }
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  //Process(s"cd ${workspacePath}/src/main/scala/EasonLib/CocoSim;python genCocoTemplate.py --target_path=${workspacePath} --simulator=verilator --vpath=${coco_path}/* --vtop=${top_name} --inputs ${input_list}" ) !

  def doit() = {
    val COCO_PATH = mutable.ArrayBuffer[String]()
    SimConfig.compile(rtl = {
      //      val top_name = "fetch_unit"
      val inputs = design.getAllIo.filter(_.isInput)
      val inputs_string = for (io <- inputs) yield io.getName()

      val top_name = design.getClass.getSimpleName.replace("$", "")
      val coco_path = workspacePath + "/cocoTest/" + top_name
      COCO_PATH += coco_path
      val file = new File(coco_path)
      val result = file.mkdirs()

      val template_path = workspacePath + "/src/main/scala/EasonLib/CocoSim/templates/"
      val makefile_path = template_path + "Makefile"
      val tb_path = template_path + "testbench.py"
//      val lib_path = "/home/lyc/projects/riscv/FlappyRiscv/src/main/scala/EasonLib/CocoSim/templates/common_drivers.py"

      val context = mutable.Map[String, Any]()
      context.put("SIMULATOR", "verilator")
      context.put("VPATH", coco_path + "/rtl/*")
      context.put("VTOP", top_name)

      gentemplate(makefile_path, coco_path + "/Makefile", context)
      context.clear()
      context.put("COM_DRIV_PATH", template_path)
      context.put("inputs",inputs_string.toArray)
      gentemplate(tb_path, coco_path + "/testbench.py", context)

      val run_path = coco_path + "/run.sh"
      writeFileNoExist(run_path, "#!/usr/bin/sh\nmake | tee coco.log")

      design
    })
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
        targetDirectory = COCO_PATH(0) + "/rtl").generate(design)

    doCmd("sh run.sh",COCO_PATH(0))
  }

}
object test_coco {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("/home/lyc/projects/riscv/FlappyRiscv/src/main/scala/EasonLib/CocoSim/templates/testbench.py")
    val template = source.mkString
    source.close()
    val context = Maps.newHashMap[String,Any]()
    context.put("inputs",Array("a","b"))

    val jinjava = new Jinjava()
    val renderedTemplate = jinjava.render(template, context);
    println(renderedTemplate)



  }
}