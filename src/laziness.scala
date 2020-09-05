import scala.math.BigInt
import scala.util.Try


object Laziness {
  private def sieve(as: LazyList[BigInt]): LazyList[BigInt] =
    as.head #:: sieve(as.tail).filter(x => x % as.head != 0)

  /**
   * Generates an infinitive list of prime numbers using the
   * Sieve of Eratosthenes.
   */
  def primes: LazyList[BigInt] = sieve(LazyList.from(2).map(BigInt(_)))

  /** Generates an infinite Fibonacci sequence */
  def fibs: LazyList[BigInt] =
    BigInt(0) #:: BigInt(0) #:: fibs.lazyZip(fibs.tail).map(_ + _)
}


object Main extends App {
  val n = Try(args.head.toInt).getOrElse(10)
  Laziness.primes.take(n).foreach(println)
}
