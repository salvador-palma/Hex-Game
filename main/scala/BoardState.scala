package Projeto
import Projeto.BoardState.{inBounds, isEmptySlot}
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
  def getInput():(Int,Int)=BoardState.retrieveInput(board)
  def hasContinuousLine(): Option[String]=unionFind.percolates(board)
  def playCPUGameState(randomState: RandomState):(BoardState,RandomState) = BoardState.randomMove(this,randomState)
}

object BoardState{
  val BlueAst = s" ${Blue}*${Reset}  "
  val RedAst = s" ${Red}*${Reset}  "
  def defineBoard(n:Integer):Board=List.fill(n)(List.fill(n)(Cells.Empty))
  @tailrec private def retrieveInput(board: Board):(Int,Int)={
    try{
      println("Where to play?")
      readLine.trim match {
        case "Q" => println("Thank you for playing!"); sys.exit(0)
        case "U" => (-1,-1)
        case str => val s = str.split(" ").map(x => x.toInt); if (inBounds(s, board) && isEmptySlot((s(0)-1,s(1)-1), board)) (s(0)-1, s(1)-1) else throw new IllegalArgumentException
      }
    }catch{
      case _ => println(s"${Main.Red}Invalid Input or Coordinate already marked, try again.${Main.Reset}")
        retrieveInput(board)
    }
  }
  @tailrec private def randomMove(boardState: BoardState, rand: RandomState): (BoardState, RandomState) = {
    val x : ((Int,Int),RandomState) = rand.nextCoords(boardState.board.size)
    if (isEmptySlot(x._1, boardState.board)){
      (play(boardState, (x._1._2, x._1._1), Cells.Red),x._2)
    }
    else randomMove(boardState, x._2)
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


