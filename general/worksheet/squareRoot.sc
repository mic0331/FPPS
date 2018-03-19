object session {
  1 + 3

  def abs(x: Double) = if (x < 0) -x else x

  // sqrt using Newton's method
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001

  // we make the absolute difference proportional to
  // the x value so we can get good estimate for big
  // and small value of x

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)
}