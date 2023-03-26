package Projeto

import Projeto.Cells.Cell
import Projeto.Main.Board

import scala.annotation.tailrec
import scala.collection.mutable.HashMap


case class UnionFind(map : List[List[Int]]){
  def isConnected(c1:(Int,Int), c2:(Int,Int)):Boolean = UnionFind.connected(c1,c2,map)
  def createUnions(c1:(Int,Int), board: Board, cell:Cell):UnionFind = UnionFind.unions(c1,map, board, cell)
  def print():Unit=UnionFind.print(map)
}
object UnionFind{
  def init(n:Int): List[List[Int]]= List.fill(n)(-1) :: (List.range(0,n) foldRight List[List[Int]](List.fill(n)(n*n)))((x,acc)=> {
    val t = -2::(List.range(x*n,x*n+n):+(-3))
    t::acc
  })
  def print(map:List[List[Int]]):Unit= map.foreach(x=> println(x))

  def connected(c1:(Int,Int), c2:(Int,Int), map : List[List[Int]]):Boolean=map(c1._1)(c1._2) == map(c2._1)(c2._2)

  def union(c1: (Int, Int), c2: (Int, Int), l: List[List[Int]]): List[List[Int]] = {
    try{
      val i2 : Int = l(c2._2+1)(c2._1+1)
      val i1 : Int = l(c1._2+1)(c1._1+1)

      if (i1 == i2) l
      val newList = l.map(innerList => innerList.map(value => if (value == i1) i2 else value))
      newList
    }catch {
      case _=> l
    }
  }
  def unions(c: (Int, Int), map: List[List[Int]], board:Board, cell:Cell):UnionFind={
    def createUnions(l: List[(Int, Int)], acc: List[List[Int]]): UnionFind = {
      l match {
        case Nil => UnionFind(acc)
        case h :: t =>{
          try{
            if (board(h._2)(h._1) != cell) {
              createUnions(t, acc)
            } else {
              val u = union(c, h, acc)
              createUnions(t, u)
            }
          }catch{
            case _ => {val u =  union(c, h, acc)
            createUnions(t, u)}
          }
        }

      }
    }
    val x = c._1
    val y = c._2
    val coordsList : List[(Int, Int)] =List((x+1,y),(x-1,y),(x,y+1),(x,y-1),(x-1,y+1),(x+1,y-1))
    createUnions(coordsList, map)

  }
}

