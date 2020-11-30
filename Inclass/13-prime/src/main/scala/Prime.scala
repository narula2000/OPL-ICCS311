object Prime extends App {
  val primes: Stream[Int] = 2 #:: Stream
    .from(3)
    .filter(i => primes.takeWhile(k => k * k <= i).forall(k => i % k > 0))

  println(primes.take(10).toList)
}
