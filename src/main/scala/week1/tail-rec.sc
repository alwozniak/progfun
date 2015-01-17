def factorial(n: Int) = {
  def factorial_aux(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorial_aux(n - 1, acc * n)

  factorial_aux(n, 1)
}

factorial(4)