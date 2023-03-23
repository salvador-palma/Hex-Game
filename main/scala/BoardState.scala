package Projeto
import Projeto.Cells.Cell
import Projeto.Main._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class BoardState(board:Board){
  def drawGameState():Unit = BoardState.drawBoard(board)
  def playGameState(coords:(Int,Int), piece : Cell):BoardState = BoardState.inputBoard(board,coords, piece)
  def getCheckInput():(Int,Int)=BoardState.retrieveInput(board)
}

object BoardState{
  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))
  @tailrec def retrieveInput(board: Board):(Int,Int)={
    println("Where to play?")
    val str = readLine.trim
    val s = str.split(" ").map(x=> x.toInt)
    if(s(0) < board.size && s(1) < board.size) return (s(0), s(1))
    if(str=="Q")sys.exit(0)
    println("Invalid Input")
    retrieveInput(board)
  }
  def inputBoard(board: Board, coords:(Int,Int), piece:Cell):BoardState={
    val b : Board = board
    BoardState(b.updated(coords._1, b(coords._1).updated(coords._2, piece)))
  }
  def drawBoard(board: Board): Unit = {
    println(s" ${Blue}*${Reset}  " * board.size)
    printRows()
    println((" " * (board.size + 1))+ s" ${Blue}*${Reset}  " * board.size)
    @tailrec def printRows(column: Integer = 0): Any = {
      @tailrec def printItems(row: Integer = 0): Any = {
        if (row >= board.size) {return}
        print(board(column)(row) + (if(row + 1 < board.size) " - " else ""))
        printItems(row + 1)
      }
      if (column >= board.size) {return}
      print((" " * column) + s"${Red}*${Reset} ");
      printItems();
      println(s" ${Red}*${Reset}")
      if (column + 1 < board.size) println((" " * (column + 2)) + ("\\ / " * (board.size - 1)) + "\\")
      printRows(column + 1)

    }
  }

}


