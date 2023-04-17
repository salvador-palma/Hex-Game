package Projeto
import Projeto.BoardState
import Projeto.Cells.Cell
import Projeto.Main.Board

trait Player {
  def getInput(boardState: BoardState):(Int,Int)
  def getPiece():Cell
}

case class Person(cell: Cell) extends Player{
  def getInput(boardState: BoardState): ((Int,Int),RandomState) = {


  }
  def getPiece():Cell=cell
}
