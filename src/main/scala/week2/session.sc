/*
 * Higher order functions.
 */

def sumFun(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sumFun(f, a + 1, b)

def sum(a: Int, b: Int)= sumFun((x: Int) => x, a, b)
sum(1,5)

def sumOfSquares(a: Int, b: Int) = sumFun((x: Int) => x * x, a, b)
sumOfSquares(1, 3)

def sumTailRec(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}
sumTailRec((x: Int) => x)(1, 3)

def sumAsFun(f: Int => Int): (Int, Int) => Int = {
  def sum(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sum(a + 1, b)
  sum
}

def product(f: Int => Int): (Int, Int) => Int = {
  def productOnInt(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * productOnInt(a + 1, b)
  productOnInt
}

def factorial(n: Int) = product((x: Int) => x)(1, n)
factorial(4)

def combiner(operator: (Int, Int) => Int, nil: Int): (Int => Int, Int, Int) => Int = {
  def aux(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) nil
    else operator(f(a), aux(f, a + 1, b))
  aux
}
combiner((x: Int, y: Int) => x + y, 0)((a: Int) => a * a, 0, 4)

/*
 * Fixed points.
 */

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  Math.abs(x - y) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 + x/2)(1.0)
def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
sqrt(2)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
def sqrt1(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
sqrt1(2)