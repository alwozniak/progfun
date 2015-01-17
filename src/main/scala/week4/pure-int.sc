/**
 * Peano numbers: implementation without primitives, objects and functions.
 */
abstract class Nat {
  def isZero: Boolean
  def pred: Nat
  def succ: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def pred = throw new Exception("Zero.pred")
  def succ = new Succ(this)
  def + (that: Nat) = that
  def - (that: Nat) = throw new Exception("subtracting from Zero")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def pred = n
  def succ = new Succ(this)
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.pred
}