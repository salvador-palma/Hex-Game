package Projeto
import Projeto.Cells.Cell
import Projeto.Main._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn.readLine
import mutable.HashMap
case class BoardState(board:Board,unionFind: UnionFind){
  def drawGameState():Unit = BoardState.drawBoard(board)
  def drawGameStateFold():String = BoardState.drawBoardFold(board)
  def playGameState(coords:(Int,Int), piece : Cell):BoardState = BoardState.inputBoard(this,coords, piece)
  def getCheckInput():(Int,Int)=BoardState.retrieveInput(board)


  def getWin():Boolean=BoardState.WinCheck(this)

}

object BoardState{
  val BlueAst = s" ${Blue}*${Reset}  "
  val RedAst = s" ${Red}*${Reset}  "
  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))
  @tailrec def retrieveInput(board: Board):(Int,Int)={
    try{
      println("Where to play?")
      readLine.trim match {
        case "Q" => println("Thank you for playing!"); sys.exit(0)
        case str => val s = str.split(" ").map(x => x.toInt); if (inBounds(s, board) && isEmptySlot(s, board)) (s(0)-1, s(1)-1) else throw new IllegalArgumentException
      }
    }catch{
      case _ => println(s"${Main.Red}Invalid Input or Coordinate already marked, try again.${Main.Reset}")
        retrieveInput(board)
    }
  }
  def inputBoard(boardState: BoardState, coords:(Int,Int), piece:Cell):BoardState={
    val b : Board = boardState.board
    val u : UnionFind = boardState.unionFind

    BoardState(b.updated(coords._1, b(coords._1).updated(coords._2, piece)), u.createUnions((coords._2, coords._1), boardState.board, piece))
  }
  def drawBoard(board: Board): Unit = {
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
  def drawBoardFold(board:Board):String={
    s"${BlueAst * board.size}\n${(List.range(0, board.size) foldRight "")((row,c)=> {
        s"${" "*row}${Red}*${Reset} ${(List.range(0,board(row).size) foldRight "")((item, acc)=>{ board(row)(item) + " - " + acc}).dropRight(3)}" + s" ${Red}*${Reset}\n${" " * (row + 2)}${("\\ / " * (board.size - 1))}\\\n${c}"
    }).dropRight((board.size-1)*5 + 3) }" +
    s"${" " * board.size}${BlueAst*board.size}"
  }
  private def inBounds(coords : Array[Int], board: Board):Boolean=coords(0) <= board.size && coords(1) <= board.size && coords(0)>0 && coords(1)>0
  private def isEmptySlot(coords:Array[Int], board:Board):Boolean=board(coords(1)-1)(coords(0)-1).equals(Cells.Empty)

  def WinCheck(boardState: BoardState): Boolean = {
    boardState.unionFind.map(0)(0) == boardState.unionFind.map(boardState.board.size + 1)(boardState.board.size - 1)
  }







}


