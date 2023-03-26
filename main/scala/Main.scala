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

    val boardState:BoardState = BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5)))
    update(boardState)




  }
  @tailrec def update(boardState: BoardState){

    boardState.drawGameState

    val (x,y) = boardState.getCheckInput

    val newBoardState : BoardState = boardState.playGameState((y,x), Cells.Blue)
    newBoardState.unionFind.print()
    newBoardState.drawGameState

    val (x2, y2) = boardState.getCheckInput

    val nBoardState: BoardState = newBoardState.playGameState((y2, x2), Cells.Red)
    nBoardState.unionFind.print()
    //Get CPU Coords
    //Play CPU Coords
    //Check for Win
    /*
    newBoardState.hasWinner match{
      case Some(x) => println("Winner Player!")
      case None=>
    }*/
    update(nBoardState)
  }

  }




