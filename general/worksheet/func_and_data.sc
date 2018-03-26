class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1) // second constructor

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  //private val g = gcd(x, y) // better to keep early simplification to avoid numerical
  // overflow

  def numer = x

  def denom = y

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def sub(that: Rational) = add(that.neg)

  def neg: Rational =
    new Rational(-this.numer, this.denom)

  /*
  val strange = new Rational(1, 0)
  strange.add(strange)
  */

  override def toString = {
    val g = gcd(x, y)
    this.numer / g + "/" + this.denom / g
  }
}

/*
val rational = new Rational(1, 2)
rational.denom

def addRational(r: Rational, s: Rational): Rational =
  new Rational(
    r.numer * s.denom + s.numer * r.denom, r.denom * s.denom
  )

def makeString(r: Rational) =
  r.numer + "/" + r.denom

makeString(addRational(new Rational(1, 2), new Rational(2, 3)))
*/
val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.add(y)
x.sub(y)
x.neg

x.sub(y).sub(z)
y.add(y)
x.less(y)
x.max(y)

new Rational(2) // make use of the second constructor



