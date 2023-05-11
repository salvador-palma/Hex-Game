package Projeto
trait RandomState {
  def nextInt(n: Int): (Int, RandomState)
  def nextCoords(n:Int):((Int,Int), RandomState)
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


}



