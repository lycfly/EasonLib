package EasonLib.Utils
object MyLog{
  def tag(name: String, color: String): String =
    if (System.console != null)
      s"[$color$name${Console.RESET}]"
    else
      s"[$name]"
}
object TimeDriver {
    val startTime = System.currentTimeMillis()
    def executionTime: Double = (System.currentTimeMillis - startTime) / 1000.0
}
object MyProgress {
  def apply(message: String) =
    println(s"${MyLog.tag("Progress", Console.BLUE)} at ${f"${TimeDriver.executionTime}%1.3f"} : $message")
}