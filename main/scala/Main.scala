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
  def main(args: Array[String]): Unit = {


    val boardState:BoardState = BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5)))
    update(boardState, Random(10))


  }
/*
  @tailrec def mainMenu(): Unit = {
    try{
      println(s"${Yellow}Welcome to Hex-Game!${Reset}\n\n1-Player vs. CPU\n2-Player vs. Player\nQ-Quit")
      readLine.trim match{
        case "1"=>update(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))))
        case "2"=>
        case _  =>
      }
    }
    mainMenu
  }*/
  @tailrec def update(boardState: BoardState, randomState: RandomState){
    boardState.drawGameState
    val (x,y) = boardState.getCheckInput

    val playerBoardState : BoardState = boardState.playGameState((y,x), Cells.Blue)
    val winP = playerBoardState.getWin
    if (winP != None) println(s"${Yellow}Conratulations, You Won!!!${Reset}")
    //playerBoardState.drawGameState

    val CPUBoardState: (BoardState,RandomState) = playerBoardState.playCPUGameState(randomState)
    val winCPU = CPUBoardState._1.getWin

    if (winCPU.contains("CPU"))) println(s"${Yellow}Oh no, you lost to CPU :(${Reset}")

    update(CPUBoardState._1, CPUBoardState._2)
  }

  }




