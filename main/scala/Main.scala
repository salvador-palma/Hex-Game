package Projeto
import Projeto.Cells

object Main{
  type Board = List[List[Cells.Cell]]
  def main(args: Array[String]): Unit = {
    val board: Board = defineBoard(5)
    printRow(board)
    print(board.size)
  }

  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))
  def printRow(board : Board, column : Integer = 0): Any = {
    def printItem(row : Integer = 0):Any={
      if(row < board.size){
        board(column)(row) match {
          case Cells.Empty => print(".")
          case Cells.Blue => print("O")
          case Cells.Red => print("X")
        }
        printItem(row+1)
      }
    }

    if(column<board.size){
      printItem()
      println("")
      printRow(board, column+1)
    }
  }
}



