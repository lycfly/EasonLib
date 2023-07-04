package EasonLib.PTPX

import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import java.nio.file.Paths
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

/**
 */
class PTPX_flow[T <: Component](
                                  topModuleName: String,
                                  workspacePath: String,
                                  PTPXConfig: PTPX_config,
                                  Date_mode: Boolean = false,
                                  designPath:String = "",
                                  start_time:Float,
                                  end_time: Float
                                ) {

  import PTPX_config._
  import PTPX_setup._

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
      run_script += "if [ ! -d 'rpt' ];then\n"
      run_script += "mkdir rpt\n fi \n"
      run_script += "if [ ! -d 'log' ];then\n"
      run_script += "mkdir log\n fi \n"
      run_script += "mkdir rpt/${log_date}\n"
      run_script += "pt_shell -f main.tcl -x \"set run_date ${log_date}\" | tee ./log/${log_date}.log\n"
      writeFile("run_core", run_script)
    } else {
      run_script += "#!/bin/sh\n"
      run_script += "log_date=test\n"
      run_script += "if [ ! -d 'rpt' ];then\n"
      run_script += "mkdir rpt\n fi \n"
      run_script += "if [ ! -d 'log' ];then\n"
      run_script += "mkdir log\n fi \n"
      run_script += "mkdir rpt/${log_date}\n"
      run_script += "pt_shell -f main.tcl -x \"set run_date ${log_date}\" | tee ./log/${log_date}.log\n"
      writeFile("run_core", run_script)
    }
    Process(s"chmod 755 $workspacePath/run_core")!

    writeFile("ptpx_setup.tcl", PTPX_setup(PTPXConfig).get_ptpx_setup(PTPXConfig))
    writeFile("main.tcl", PTPX_setup(PTPXConfig).get_ptpx_main(PTPXConfig, topModuleName, designPath,start_time, end_time))

    doCmd("./run_core",workspacePath)


  }
}
