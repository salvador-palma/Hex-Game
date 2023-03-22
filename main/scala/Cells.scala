package Projeto

import Projeto.Cells.Cell

object Cells extends Enumeration {
  type Cell = Value
  val Red = Value("X")
  val Empty = Value(".")
  val Blue = Value("O")
}



