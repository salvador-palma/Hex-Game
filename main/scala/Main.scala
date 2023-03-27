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
  val Yellow = "\u001B[33m"
  val Bold = "\u001B[1m"
  def main(args: Array[String]): Unit = {
    mainMenu()
  }

  @tailrec def mainMenu(){
    println(s"      ${Bold}${Red}HEX${Reset}-${Blue}GAME!${Reset}\n\n${Bold}     Main  Menu${Reset}\n\n1-Player vs. CPU (easy)\n2-Player vs. Player\nQ-Quit")
    readLine.trim.toUpperCase match{
      case "1"=>update(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))), Random(10))
      case "2"=>println("Not yet implemented")
      case "Q"=>println("\n See you next time!")
      case _  =>println("Invalid Option!")
    }
    mainMenu
  }
  @tailrec def update(boardState: BoardState, randomState: RandomState){
    boardState.drawGameState
    val (x,y) = boardState.getCheckInput
    val playerBoardState : BoardState = boardState.playGameState((y,x), Cells.Blue)
    val CPUBoardState: (BoardState,RandomState) = playerBoardState.playCPUGameState(randomState)

    CPUBoardState._1.hasContinuousLine match{
      case Some("P1")=> boardState.drawGameState; println(s"${Yellow}Conratulations, You Won!!!${Reset}")
      case Some("P2")=>  println(s"${Yellow}Oh no, you lost to CPU :(${Reset}")
      case _=> update(CPUBoardState._1, CPUBoardState._2)
    }
  }
  }




