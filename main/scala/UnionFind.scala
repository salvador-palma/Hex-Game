package Projeto

import scala.collection.mutable.HashMap


case class UnionFind(map : List[Int], n:Int){
  def isConnected(c1:(Int,Int), c2:(Int,Int)):Boolean = UnionFind.connected(c1,c2,map,n)
  def createUnion(c1:(Int,Int), c2:(Int,Int)):UnionFind = UnionFind.union(c1,c2,map,n)
}

object UnionFind{
  def init(n:Int): List[Int]=List.range(0, n*n)

  def connected(c1:(Int,Int), c2:(Int,Int), map : List[Int], n:Int):Boolean=map((c1._1 * n) + c1._2) == map((c2._1 * n) + c2._2)

  def union(c1: (Int, Int), c2: (Int, Int), map: List[Int], n: Int): UnionFind = {
    val i1 = (c1._1 * n) + c1._2
    val i2 = (c2._1 * n) + c2._2
    val p1 = map(i1)
    val newMap: List[Int] = map.map(x => if (x == p1) map(i2) else x)
    UnionFind(newMap, n)
  }
}

