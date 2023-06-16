package Hex
trait GameRandomState {
  def nextInt(n: Int): (Int, GameRandomState)
}

case class GameRandom(seed: Long) extends GameRandomState {
  def nextInt(n: Int): (Int, GameRandomState) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = GameRandom(nextSeed)
    val value = ((nextSeed >>> 16).toInt) % n
    (if (value < 0) -value else value, nextRandom)
  }
}