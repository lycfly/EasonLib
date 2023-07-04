package EasonLib.DesignCompiler

import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import java.nio.file.Paths
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

/**
 */
class DesignCompilerFlow[T <: Component](
                                  design: => T,
                                  topModuleName: String,
                                  workspacePath: String,
                                  DCConfig: DesignCompiler_config,
                                  Date_mode: Boolean = false,
                                  force: Boolean = false,
                                  sdcPath: String = "",
                                  designPath:String = ""
                                ) {

  import DesignCompiler_config._
  import DesignCompiler_setup._

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

//  def getScript(DCConfig: DesignCompiler_config, rtlSources: mutable.LinkedHashSet[String]) = {
//    var script = ""
//
//    if(designPath.isEmpty) rtlSources.map(_.replace(workspacePath + "/", "")).foreach { path =>
//      println(designPath)
//      if (path.endsWith(".sv")) script += s"read_verilog -sv $path \n"
//      else if (path.endsWith(".v")) script += s"read_verilog $path \n"
//      else if (path.endsWith(".vhdl") || path.endsWith(".vhd")) script += s"read_vhdl $path \n"
//      else if (path.endsWith(".bin")) Unit
//      else throw new IllegalArgumentException(s"invalid RTL source path $path")
//    }
//    else {
//      script += s"read_verilog -v $designPath \n"
//    }
//    script += (if (xdcPath.nonEmpty) s"read_xdc $xdcPath\n" else s"read_xdc doit.xdc\n")
//
//    script
//  }


  def doit() = {

    // prepare the workspace
    val workspacePathFile = new File(workspacePath)
    if (workspacePathFile.listFiles != null) {
     // assert(force, "the workspace is not empty, to flush it anyway, using \"force = true\"")
     // FileUtils.deleteDirectory(workspacePathFile)
      println("Workspace exist !")
    } else {
      Process(s"mkdir -p $workspacePath") !      // create directory in this way instead
    }
    var run_script = ""
    if(Date_mode){
      run_script += "#!/bin/sh\n"
      run_script += "log_date=$(date +%m%d)_$(date +%H%M)\n"
      run_script += "mkdir log\n"
      run_script += "mkdir outputs\n"
      run_script += "mkdir rpt\n"
      run_script += "mkdir outputs/$log_date\n"
      run_script += "mkdir rpt/$log_date\n"
      run_script += "dc_shell-t -f core.tcl -x \"set date ${log_date}\" | tee ./log/core_${log_date}.log\n"
      writeFile("run_core", run_script)
    } else {
      run_script += "#!/bin/sh\n"
      run_script += "log_date=test\n"
      run_script += "mkdir log\n"
      run_script += "mkdir outputs\n"
      run_script += "mkdir rpt\n"
      run_script += "mkdir outputs/$log_date\n"
      run_script += "mkdir rpt/$log_date\n"
      run_script += "dc_shell-t -f core.tcl -x \"set date ${log_date}\" | tee ./log/core_${log_date}.log\n"
      writeFile("run_core", run_script)
    }
    Process(s"chmod 755 $workspacePath/run_core")!
    // generate verilog
   // val spinalReport = SpinalConfig(targetDirectory = workspacePath).generate(design.setDefinitionName(topModuleName))
    val spinalReport = SpinalConfig(
     defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
       clockEdge = RISING,
       resetActiveLevel = LOW),
       mode=Verilog,
       targetDirectory = workspacePath
      // oneFilePerComponent = true
     )
     //.addStandardMemBlackboxing(blackboxAll)
     .generate(design.setDefinitionName(topModuleName))

    println(spinalReport.rtlSourcesPaths)
    println("rtlsource ")
    writeFile("dc_setup.tcl", DesignCompiler_setup(DCConfig).get_dc_setup(DCConfig))
    writeFile("constraint.tcl", DesignCompiler_setup(DCConfig).get_dc_constraint(DCConfig))
    writeFile("core.tcl", DesignCompiler_setup(DCConfig).get_dc_core(DCConfig,topModuleName,spinalReport.rtlSourcesPaths.head))

    doCmd("./run_core",workspacePath)


//    new VivadoReport(workspacePath, xilinxDeviceFamily, frequencyTarget)
  }
}

//// TODO: modify the API, name and path should be passed to the Flow, so the user can use Config and Task from the recommendation
//object VivadoFlow {
//  def apply[T <: Component](
//                             design: => T,
//                             topModuleName: String,
//                             workspacePath: String,
//                             vivadoConfig: VivadoConfig = defaultVivadoConfig,
//                             vivadoTask: VivadoTask = VivadoTask(),
//                             force: Boolean = true,
//                             xdcPath: String = "",
//                             designPath: String = ""
//                           ) = new VivadoFlow(design, topModuleName, workspacePath, vivadoConfig, vivadoTask, force, xdcPath, designPath)
//
//  def main(args: Array[String]): Unit = {
//    import Chainsaw._
//    VivadoSynth("/home/ltr/IdeaProjects/Chainsaw/src/main/scala/Chainsaw/temp.sv")
//  }
//}