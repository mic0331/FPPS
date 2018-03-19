import scala.annotation.tailrec

object session {

  def factioral(n: Int): Int =
    if (n == 0) 1 else n * factioral(n - 1)

  factioral(4)

  // gcd present a tail-recursion as the function call itself
  // as it's last action --> the function's stack can be reused.
  // factioral is not following this scheme because n is multiply by
  // the function itself.
  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)

  // factioral tail recursive implementation
  def factorial_tr(n: Int): Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  factorial_tr(4)
}