object Prime extends App {
  def factorial(n: Int): Long = {
    def loop(n: Int, accum: Long): Long = {
      if (n <= 1) accum
      else loop(n - 1, n * accum)
    }
    loop(n, 1)
  }

  def genPrime(n: Int): LazyList[Int] = {
    def checkPrime(num: Int): Boolean = {
      num > 1 && !(2 to (num - 1)).exists(x => num % x == 0)
    }
    if (checkPrime(n)) n #:: genPrime(n + 1)
    else genPrime(n + 1)
  }
}
