trait RandomState {
  def nextInt: (Int, RandomState)
  def nextInt(n: Int): (Int, RandomState)
}

case class Random(seed: Long) extends RandomState {

  def nextInt: (Int, RandomState) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = Random(nextSeed)
    val value = (nextSeed >>> 16).toInt
    (value, nextRandom)
  }

  def nextInt(n: Int): (Int, RandomState) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = Random(nextSeed)
    val value = ((nextSeed >>> 16).toInt) % n
    (if (value < 0) -value else value, nextRandom)
  }

}



