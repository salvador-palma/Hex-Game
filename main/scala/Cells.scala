package Projeto

object Cells extends Enumeration {
  type Cell = Value
  val Red = Value(s"${Main.Red}X${Main.Reset}")
  val Empty = Value(".")
  val Blue = Value(s"${Main.Blue}O${Main.Reset}")

  def reverse(cell: Cell):Cell={
    cell match{
      case Red=>Blue
      case Blue=>Red
    }
  }
}



