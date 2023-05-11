package Projeto
object Cells extends Enumeration {
  type Cell = Value
  val Red = Value(s"${MainTUI.Red}X${MainTUI.Reset}")
  val Empty = Value(".")
  val Blue = Value(s"${MainTUI.Blue}O${MainTUI.Reset}")

  def reverse(cell: Cell):Cell={
    cell match{
      case Red=>Blue
      case Blue=>Red
    }
  }
}



