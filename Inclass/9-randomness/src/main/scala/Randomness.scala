trait RandomnessService {
  def nextRandomNum(): Int
}

trait ConstantRandom extends RandomnessService {
  def nextRandomNum() = 49
}

trait IncrementRandom extends RandomnessService {
  var num = 49
  def nextRandomNum() = {
    num = num + 1
    num
  }
}
trait SeedProvider {
  def RandomSeed: Long
}

trait PseudoRandom extends RandomnessService {
  this: SeedProvider =>
  val RNG = new scala.util.Random(RandomSeed)
  def nextRandomNum() = RNG.nextInt()
}

class LuckyDraw {
  this: RandomnessService =>
  def showLuckyNumbers(n: Int) = {
    def generator(n: Int, C: List[Int] => List[Int]): List[Int] =
      n match {
        case 0 => C(Nil)
        case _ => generator(n - 1, (K: List[Int]) => C(n :: K))
      }
    val luckyNumbers =
      generator(n, (x: List[Int]) => x).map(k => this.nextRandomNum())
    luckyNumbers.foreach(println(_))
  }
}
