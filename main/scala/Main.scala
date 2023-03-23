package Projeto
import Projeto.BoardState._
import Projeto.Main.Board
import scala.annotation.tailrec
import scala.io.StdIn._
object Main{
  type Board = List[List[Cells.Cell]]
  val Red = "\u001b[31m"
  val Blue = "\u001b[34m"
  val Reset = "\u001b[0m"
  def main(args: Array[String]): Unit = {


    val boardState:BoardState = BoardState(BoardState.defineBoard(5))
    update(boardState)


  }
  //@tailrec
  def update(boardState: BoardState){
    boardState.drawGameState
    println("Keep playing? Y/N")
    readLine match{case "N" => return
    case _ => }
    println("X coord:")
    val x = readInt
    println("Y coord:")
    val y = readInt
    val newBoardState : BoardState = boardState.playGameState((y,x), Cells.Blue)
    update(newBoardState)
  }



  }




