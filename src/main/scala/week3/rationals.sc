class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non 0")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int =
    if (y == 0) x
    else gcd(y, x % y)
  private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  def + (that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def - (that: Rational) = this + (-that)

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this < that) that
    else this

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x - y - z