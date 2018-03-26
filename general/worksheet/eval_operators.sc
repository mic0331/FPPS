// infix operator

class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1) // second constructor

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g

  def denom = y / g

  def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def - (that: Rational) = this + -that

  def unary_- : Rational =
    new Rational(-this.numer, this.denom)

  override def toString = {
    this.numer + "/" + this.denom
  }
}


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x + y
x - y
-x

x + y - z
y + y
x < y
x max y