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
  val Green = "\u001B[32m"
  def main(args: Array[String]): Unit = {
    mainMenu()
  }

  @tailrec def mainMenu(){
    println(s"      ${Bold}${Red}HEX${Reset}-${Blue}GAME!${Reset}\n\n${Bold}     Main  Menu${Reset}\n\n1-Player vs. CPU (easy)\n2-Player vs. Player\nQ-Quit")
    readLine.trim.toUpperCase match{
      case "1"=>update(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))), Random(10), List.empty[BoardState])
      case "2"=>println("Not yet implemented")
      case "Q"=>println("\n See you next time!")
      case _  =>println("Invalid Option!")
    }
    mainMenu
  }
  @tailrec def update(boardState: BoardState, randomState: RandomState, history:List[BoardState]){
    boardState.draw
    boardState.getInput match {
      case (-1,-1)=> {
        history match{
          case Nil=>println(s"${Red}There are no moves to Undo${Reset}");update(boardState,randomState, history)
          case h::t=>println(s"${Green}Move undone${Reset}");update(h,randomState,t)
        }
      }
      case (x:Int,y:Int)=>  {
        val playerBoardState: BoardState = boardState.playGameState((y, x), Cells.Blue)
        val CPUBoardState: (BoardState, RandomState) = playerBoardState.playCPUGameState(randomState)
        CPUBoardState._1.hasContinuousLine match {
          case Some("P1") => playerBoardState.draw; println(s"${Yellow}Conratulations, You Won!!!${Reset}")
          case Some("P2") => CPUBoardState._1.draw; println(s"${Yellow}Oh no, you lost to CPU :(${Reset}")
          case _ => update(CPUBoardState._1, CPUBoardState._2, boardState::history)
        }
      }
    }
  }
  }




