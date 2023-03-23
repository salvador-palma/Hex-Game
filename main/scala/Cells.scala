package Projeto

import Projeto.Cells.Cell

object Cells extends Enumeration {
  type Cell = Value
  val Red = Value(s"${Main.Red}X${Main.Reset}")
  val Empty = Value(".")
  val Blue = Value(s"${Main.Blue}O${Main.Reset}")
}



