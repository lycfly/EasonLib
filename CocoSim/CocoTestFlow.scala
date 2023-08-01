import scala.io.Source
import scala.sys.process._


class CocoTestFlow[T <: Component](
                                  design: => T,
                                  topModuleName: String,
                                  workspacePath: String,
                                  CoCoConfig: DesignCompiler_config,
                                  force: Boolean = false,
                                  designPath:String = ""
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

  
}
