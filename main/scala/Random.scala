package Projeto
trait RandomState {
  def nextInt(n: Int): (Int, RandomState)
  def nextCoords(n:Int):((Int,Int), RandomState)
  def doubleList(n:Int,r:(Int,Int)):(List[Double],RandomState)
}

case class Random(seed: Long) extends RandomState {
  def nextInt(n: Int): (Int, RandomState) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = Random(nextSeed)
    val value = ((nextSeed >>> 16).toInt) % n
    (if (value < 0) -value else value, nextRandom)
  }
  def nextCoords(n:Int):((Int,Int),RandomState)={
    val x = nextInt(5)
    val x1 = x._2.nextInt(5)
    ((x._1,x1._1),x1._2)
  }
  def doubleList(length: Int, range : (Int,Int)): (List[Double], RandomState) = {
    def loop(count: Int, state: RandomState, acc: List[Double]): (List[Double], RandomState) = {
      if (count == 0) (acc.reverse, state)
      else {
        val (randomInt, nextState) = state.nextInt(1000000)
        loop(count - 1, nextState, randomInt.toDouble / 1000000 :: acc)
      }
    }
    val r = loop(length, this, Nil)
    val m = r._1.map(x=>x*(range._2 - range._1) + range._1)
    (m,r._2)
  }


}



