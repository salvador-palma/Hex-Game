package Projeto
import Projeto.BoardState._
import Projeto.Cells.Cell
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

    val network = GenerateSuperAI(1000)
    PlaySuperAI(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))), network)

  }
  @tailrec def mainMenu(){
    println(s"      ${Bold}${Red}HEX${Reset}-${Blue}GAME!${Reset}\n\n${Bold}     Main  Menu${Reset}\n\n1-Player vs. CPU (easy)\n2-Player vs. Player\nQ-Quit")
    readLine.trim.toUpperCase match{
      case "1"=>updatePvC(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))), Random(10));mainMenu
      case "2"=>updatePvP(BoardState(BoardState.defineBoard(5), UnionFind(UnionFind.init(5))));mainMenu
      case "Q"=>println("\n See you next time!");
      case _  =>println("Invalid Option!");mainMenu
    }

  }
  @tailrec def updatePvC(boardState: BoardState, randomState: RandomState, history:List[BoardState] = List.empty[BoardState]){
    boardState.draw
    boardState.getInput match {
      case (-1,-2)=>{println(s"${Yellow}Thank you for playing!${Reset}")}
      case (-1,-1)=> {
        history match{
          case Nil=>println(s"${Red}There are no moves to Undo${Reset}");updatePvC(boardState,randomState, history)
          case h::t=>println(s"${Green}Move undone${Reset}");updatePvC(h,randomState,t)
        }
      }
      case (x,y)=>  {
        val playerBoardState: BoardState = boardState.playGameState((y, x), Cells.Blue)
        val ((cpuX,cpuY), nextRand) = playerBoardState.playCPUGameState(randomState)
        val CPUBoardState: BoardState = playerBoardState.playGameState((cpuY,cpuX), Cells.Red)
        CPUBoardState.hasContinuousLine match {
          case Some("P1") => playerBoardState.draw; println(s"${Yellow}Conratulations, You Won!!!${Reset}")
          case Some("P2") => CPUBoardState.draw; println(s"${Yellow}Oh no, you lost to CPU :(${Reset}")
          case _ => updatePvC(CPUBoardState, nextRand, boardState::history)
        }
      }
    }
  }
  @tailrec def updatePvP(boardState: BoardState, history: List[BoardState] = List.empty[BoardState], currentPlayer : Cell = Cells.Blue) {
    boardState.draw
    boardState.getInput match {
      case (-1, -2) => println(s"${Yellow}Thank you for playing!${Reset}")
      case (-1, -1) =>
        history match {
          case Nil => println(s"${Red}There are no moves to Undo${Reset}"); updatePvP(boardState, history)
          case h :: t => println(s"${Green}Move undone${Reset}"); updatePvP(h, t)
        }
      case (x, y) => {
        val playerBoardState: BoardState = boardState.playGameState((y, x), currentPlayer)
        val nextPlayer = if(currentPlayer.equals(Cells.Blue)) Cells.Red else Cells.Blue
        playerBoardState.hasContinuousLine match {
          case Some("P1") => playerBoardState.draw; println(s"${Yellow}Conratulations, Player 1 Won!!!${Reset}")
          case Some("P2") => playerBoardState.draw; println(s"${Yellow}Conratulations, Player 2 Won!!!${Reset}")
          case _ => updatePvP(playerBoardState, boardState :: history, nextPlayer)
        }
      }
    }
  }

  def GenerateSuperAI(level:Int): NeuralNetwork = {
    println("Generating Super AI... (This process can take some time)")
    val initialPopulation:Generation = Generation.Start(64,(-100,100),Random(234))
    def nGen(n:Int, acc:Generation):Generation={
      def nextGen(generation: Generation): Generation = {
        //println("Selecting...")
        val s = generation.Select()
        //println("Selected " + s.size)
        val g =generation.Breed(s)
        //println("Breeded to " + g.population.size)
        g
      }
      //println("Generation " + (level-n))
      n match{
        case 0=> acc
        case x=> nGen(x-1, nextGen(acc))
      }
    }
    val ngen = nGen(level,initialPopulation)
    println("Super AI Level " + level + " successfully generated")
    ngen.Strongest
  }

  def PlaySuperAI(boardState: BoardState,neuralNetwork: NeuralNetwork):Unit={
    boardState.draw
    boardState.getInput match {
      case (x, y) => {
        val playerBoardState = boardState.playGameState((y, x), Cells.Blue)
        val (cpuX, cpuY) = neuralNetwork.Predict(playerBoardState)
        val CPUBoardState = playerBoardState.playGameState((cpuY, cpuX), Cells.Red)
        CPUBoardState.hasContinuousLine match {
          case Some("P1") => playerBoardState.draw; println(s"${Yellow}Conratulations, You Won!!!${Reset}")
          case Some("P2") => CPUBoardState.draw; println(s"${Yellow}Oh no, you lost to CPU :(${Reset}")
          case _ => PlaySuperAI(CPUBoardState, neuralNetwork)
        }
      }
    }
  }




  }




