package Projeto
import Projeto.BoardState.{getRandomInput, inBounds, isEmptySlot}
import Projeto.Cells.Cell
import Projeto.Main._
import Projeto.Random._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn.readLine
import mutable.HashMap

case class BoardState(board:Board,unionFind: UnionFind){
  def draw():Unit = BoardState.drawBoard(board)
  def drawFold():String = BoardState.drawBoardFold(board)
  def playGameState(coords:(Int,Int), piece : Cell):BoardState = BoardState.play(this,coords, piece)
  def getInput():(Int,Int)=BoardState.getInput(board)
  def playCPUGameStateAdjacent(randomState: RandomState,oldCoord : (Int,Int)):((Int,Int),RandomState)= BoardState.adjacentCoord(oldCoord,this,randomState)
  def hasContinuousLine(): Option[String]=unionFind.percolates(board)
  def playCPUGameState(randomState: RandomState):((Int,Int),RandomState)= BoardState.getRandomInput(this,randomState)
  def valid(input:(Int,Int)):Boolean= BoardState.inBounds(Array(input._1 + 1,input._2 + 1),board) && BoardState.isEmptySlot(input,board)

}

object BoardState{
  val BlueAst = s" ${Blue}*${Reset}  "
  val RedAst = s" ${Red}*${Reset}  "
  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))
  @tailrec private def getInput(board: Board):(Int,Int)={
    try{
      println("Where to play?")
      readLine.trim match {
        case "Q" => (-1,-2)
        case "U" => (-1,-1)
        case str => val s = str.split(" ").map(x => x.toInt); if (inBounds(s, board) && isEmptySlot((s(0)-1,s(1)-1), board)) (s(0)-1, s(1)-1) else throw new IllegalArgumentException
      }
    }catch{
      case _ => println(s"${Main.Red}Invalid Input or Coordinate already marked, try again.${Main.Reset}")
        getInput(board)
    }
  }
  @tailrec private def getRandomInput(boardState: BoardState, rand: RandomState): ((Int, Int),RandomState) = {
    val x : ((Int,Int),RandomState) = rand.nextCoords(boardState.board.size)
    if (isEmptySlot(x._1, boardState.board)) x
    else getRandomInput(boardState, x._2)

  }
  private def adjacentCoord(coord : (Int,Int) , boardState: BoardState, rand : RandomState) : ((Int,Int),RandomState) = {
    if (coord == (-1,-1)){
      val t = getRandomInput(boardState, rand)
      t
    }
    else {
      val x = coord._1
      val y = coord._2
      val adjList: List[(Int, Int)] = List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x - 1, y + 1), (x + 1, y - 1))
      val checkAdj = (adjList foldRight true) ( (e,acc) => !boardState.valid(e) && acc)
      if (checkAdj) {
        adjacentCoord((-1,-1),boardState, rand)
      }
      else{
        val nextRand = rand.nextInt(adjList.size)
        val randomCoord = adjList(nextRand._1)
        if (boardState.valid(randomCoord)) (randomCoord, nextRand._2) else adjacentCoord(coord, boardState, nextRand._2)
      }
    }
  }



  private def play(boardState: BoardState, coords:(Int,Int), player:Cell):BoardState={
    val b : Board = boardState.board
    val u : UnionFind = boardState.unionFind

    BoardState(b.updated(coords._1, b(coords._1).updated(coords._2, player)), u.createUnions((coords._2, coords._1), boardState.board, player))
  }
  private def drawBoard(board: Board): Unit = {
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
  private def drawBoardFold(board:Board):String={
    s"${BlueAst * board.size}\n${(List.range(0, board.size) foldRight "")((row,c)=> {
        s"${" "*row}${Red}*${Reset} ${(List.range(0,board(row).size) foldRight "")((item, acc)=>{ board(row)(item) + " - " + acc}).dropRight(3)}" + s" ${Red}*${Reset}\n${" " * (row + 2)}${("\\ / " * (board.size - 1))}\\\n${c}"
    }).dropRight((board.size-1)*5 + 3) }" +
    s"${" " * board.size}${BlueAst*board.size}"
  }
  private def inBounds(coords : Array[Int], board: Board):Boolean=coords(0) <= board.size && coords(1) <= board.size && coords(0)>0 && coords(1)>0
  private def isEmptySlot(coords:(Int,Int), board:Board):Boolean=board(coords._2)(coords._1).equals(Cells.Empty)

}


