package Hex
import scala.io.AnsiColor.{BOLD, RED, BLUE, RESET}

object Cells extends Enumeration {
  type Cell = Value
  val Red, Blue, Empty = Value
  def boardSymbol(cell: Cell): String={
    cell match {
      case Red => s"${BOLD}${RED}X${RESET}"
      case Blue => s"${BOLD}${BLUE}O${RESET}"
      case Empty => s"${BOLD}.${RESET}"
    }
  }
}
