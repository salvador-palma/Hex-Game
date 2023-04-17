package Projeto
import Projeto.BoardState
import Projeto.Main.Board

trait Player {
  def getInput(boardState: BoardState):(Int,Int)
}

case class Person() extends Player{
  def getInput(boardState: BoardState): (Int,Int) = {


  }
}
