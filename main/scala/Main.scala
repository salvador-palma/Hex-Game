package Projeto
import Projeto.BoardState._
import Projeto.Main.Board

import scala.annotation.tailrec
import scala.collection.immutable.Set.EmptySet
import scala.io.StdIn._
object Main{
  type Board = List[List[Cells.Cell]]
  val Red = "\u001b[31m"
  val Blue = "\u001b[34m"
  val Reset = "\u001b[0m"
  def main(args: Array[String]): Unit = {

    val boardState:BoardState = BoardState(BoardState.defineBoard(5))

    //boardState.drawGameState
    val s = Set.empty[(Int,Int)]
    val r = s + ((2,0))
    println(s)
    println(r)
    update(boardState)



  }
  @tailrec def update(boardState: BoardState){

    boardState.drawGameState
    boardState.hasWinner
    val (x,y) = boardState.getCheckInput

    val newBoardState : BoardState = boardState.playGameState((y,x), Cells.Blue)

    //Get CPU Coords
    //Play CPU Coords
    //Check for Win
    /*
    newBoardState.hasWinner match{
      case Some(x) => println("Winner Player!")
      case None=>
    }*/
    update(newBoardState)
  }

  }




