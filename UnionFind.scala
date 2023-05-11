package Projeto
import Projeto.Cells.Cell
import Projeto.MainTUI.Board
case class UnionFind(field : List[List[Int]]){
  def createUnions(c1:(Int,Int), board: Board, cell:Cell):UnionFind = UnionFind.unions(c1,field, board, cell)
  def percolates(board: Board):Option[String]= UnionFind.percolateCheck(field, board)
}
object UnionFind{
  def init(n:Int): List[List[Int]]= (List.range(0,n) foldRight List[List[Int]]())((x,acc)=> List.range(x*n,x*n+n) :: acc)
  private def union(c1: (Int, Int), c2: (Int, Int), l: List[List[Int]]): List[List[Int]] = {
    try{
      val i2 : Int = l(c2._2)(c2._1)
      val i1 : Int = l(c1._2)(c1._1)
      if (i1 == i2) l
      val newList = l.map(innerList => innerList.map(value => if (value == i1) i2 else value))
      newList
    }catch {
      case _=> l
    }
  }
  private def unions(c: (Int, Int), field: List[List[Int]], board:Board, cell:Cell):UnionFind={
    def createUnions(l: List[(Int, Int)], acc: List[List[Int]]): UnionFind = {
      l match {
        case Nil => UnionFind(acc)
        case h :: t =>
          try{
            if (board(h._2)(h._1) != cell) createUnions(t, acc)
            else createUnions(t, union(c, h, acc))
          }catch{
            case _ => createUnions(t, union(c, h, acc))
          }
      }
    }
    val (x,y) = c
    createUnions(List((x+1,y),(x-1,y),(x,y+1),(x,y-1),(x-1,y+1),(x+1,y-1)), field)
  }
  private def percolateCheck(field:List[List[Int]], board: Board):Option[String]={
    def hasVertical():Boolean= (List.range(0, field.size) foldRight false)((x, acc)=> (List.range(0, field.size) foldRight false)((y, acc2)=> (board(0)(x).equals(Cells.Blue) && field(0)(x) == field(field.size-1)(y))|| acc2) ||acc)
    def hasHorizontal(): Boolean = (List.range(0, field.size) foldRight false)((x, acc) => (List.range(0, field.size) foldRight false)((y, acc2) => (board(x)(0).equals(Cells.Red) && field(x)(0) == field(y)(field.size - 1)) || acc2) || acc)
    if (hasVertical) Some("P1")
    else if (hasHorizontal) Some("P2")
    else None
  }
}

