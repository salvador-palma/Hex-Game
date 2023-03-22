package Projeto
import Projeto.Cells

object Main{
  type Board = List[List[Cells.Cell]]
  val Red = "\u001b[31m"
  val Blue = "\u001b[34m"
  val Reset = "\u001b[0m"
  def main(args: Array[String]): Unit = {
    val board: Board = defineBoard(5)
    drawBoard(board)
  }

  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))
  def drawBoard(board:Board): Unit = {
    println(s" ${Blue}*${Reset}  " * board.size)
    printRow()
    print(" " * (board.size + 1))
    println(s" ${Blue}*${Reset}  " * board.size)

    def printRow(column: Integer = 0): Any = {

        def printItem(row: Integer = 0): Any = {
          if (row < board.size) {
            board(column)(row) match {
              case Cells.Empty => print(".")
              case Cells.Blue => print("O")
              case Cells.Red => print("X")
            }
            if (row + 1 < board.size) print(" - ")
            printItem(row + 1)
          }
        }

        if (column < board.size) {
          print(" " * column);
          print(s"${Red}*${Reset} ");
          printItem();
          println(s" ${Red}*${Reset}")
          if (column + 1 < board.size) println((" " * (column + 2)) + ("\\ / " * (board.size - 1)) + "\\")
          printRow(column + 1)
        }
    }
  }
  }




