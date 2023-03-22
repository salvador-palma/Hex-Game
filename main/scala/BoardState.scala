package Projeto
import Projeto.Cells.Cell
import Projeto.Main._

import scala.annotation.tailrec

case class BoardState(board:Board){
  def drawGameState():Unit = BoardState.drawBoard(board)

}

object BoardState{
  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))

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


