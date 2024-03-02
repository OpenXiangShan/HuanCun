package huancun.utils

object PrtParam {
  val ANSI_RESET = "\u001B[0m"
  val ANSI_RED = "\u001B[31m"
  val ANSI_GREEN = "\u001B[32m"
  val ANSI_YELLOW = "\u001B[33m"
  val ANSI_BLUE = "\u001B[34m"
  val ANSI_PURPLE = "\u001B[35m"
  val ANSI_CYAN = "\u001B[36m"
  val ANSI_WHITE = "\u001B[37m"

  def apply(foo: String) = {
    println(s"${ANSI_GREEN}Param ${ANSI_RESET} ${foo}")
  }
}
