/**
 * Let's prove that even primitive types can be defined in terms of objects.
 */
abstract class PureBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  // for PureTrue: return x (because in this case the result of this operation
  // depends solely on a given parameter),
  // for PureFalse: return PureFalse
  def && (x: => PureBoolean): PureBoolean = ifThenElse(x, PureFalse)

  // for PureTrue return PureTrue,
  // for PureFalse return x (this is the parameter that decides about the result of this operation)
  def || (x: => PureBoolean): PureBoolean = ifThenElse(PureTrue, x)

  // return the opposite
  def unary_! : PureBoolean = ifThenElse(PureFalse, PureTrue)

  // for PureTrue: return x (because if x is PureTrue, then the result should also be true
  // and if x is false -- it should be PureFalse)
  // for PureFalse: if an argument is PureFalse, the result should be true, so return the opposite.
  // Otherwise return false (because PureFalse != PureTrue).
  def == (x: PureBoolean): PureBoolean = ifThenElse(x, x.unary_!)

  // Analogous reasoning that we have for equality.
  def != (x: PureBoolean): PureBoolean = ifThenElse(x.unary_!, x)

  def < (x: PureBoolean): PureBoolean = ifThenElse(PureFalse, x)
}

/**
 * Just like defining true in lambda calculus.
 */
object PureTrue extends PureBoolean {
  def ifThenElse[T](t: => T, e: => T) = t
}

/**
 * Just like defining false in lambda calculus.
 */
object PureFalse extends PureBoolean {
  def ifThenElse[T](t: => T, e: => T) = e
}