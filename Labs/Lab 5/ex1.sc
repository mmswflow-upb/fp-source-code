trait Nat {
  def isZero: Boolean
  def add(other: Nat): Nat
  def subtract(other: Nat): Nat
  def greater(other: Nat): Boolean
  def toInt: Int
}

case object Zero extends Nat {
  override def isZero: Boolean = true // Zero is always zero
  override def add(other: Nat) : Nat = other // adding any number to zero gives the other number
  override def subtract(other: Nat): Nat = Zero // subtracting any number from zero gives a negative number, but we're in the natural set of numbers so we return zero
  override def greater(other: Nat) : Boolean = false  // zero isn't greater than any natural number
  override def toInt: Int = 0 // Zero -> 0
}

case class Succ(n: Nat) extends Nat {

  override def isZero: Boolean = false // Successor of anything cant be zero Succ(_) != Zero

  /*
  Destructure the first number (this current number) recursively and apply Succ on the other number to increment it by 1 on each step, until the first number
  reaches zero, so there's nothing more to add to the other number.
  */
  override def add(other: Nat) : Nat = n.add(Succ(other))

  /*
    Destructure the second natural number (decrement it by 1 on each step) together with the first number, if the second number reaches Zero first, that means that we
    have something like Succ(_).subtract(Zero) so the first number was bigger than the 2nd one. If the first number reaches zero before the 2nd one then we will have
    Zero.subtract(Succ(_)) which will always return zero
  */
  override def subtract(other: Nat) : Nat = {
    other match {
      case Succ(mp) => n.subtract(mp)
      case Zero => Succ(n)
    }
  }

  /*
  Just like subtract function, except that when the other number reaches zero before the first number, we return zero because we would have something like Succ(_).subtract(Zero)
  */
  override def greater(other: Nat) : Boolean = {
    other match {
      case Succ(mp) => n.greater(mp)
      case Zero => true
    }
  }

  /*
  Destructure the current (this) natural number recursively and add 1 at each step, until we reach 'Zero'
  */
  override def toInt: Int = {
    1 + n.toInt
  }
}

val z1 = Zero
z1.toInt // must be zero

val z2 = Zero
val z3 = z1.add(z2)
z3.toInt // must be zero

val z4 = z1.subtract(z2)
z4.toInt // must be zero

z4.greater(z1) // must be false

val n1 = Succ(Zero)
n1.toInt // must be 1
val n2 = Succ(Succ(Succ(n1)))
n2.toInt // must be 3 + 1 = 4

val n3 = n1.add(n2)
n3.toInt // must be 5

val n4 = n3.subtract(z1)
n4.toInt // must be 5 still

val n5 = n3.subtract(n1)
n5.toInt // should be 5 - 1 = 4

val n6 = z1.subtract(n5)
n6.toInt // should be 0 - 4 = 0 (we can only have natural numbers)

val n7 = n5.subtract(n2)
n7.toInt // should be 4- 4 = 0

n3.greater(n1) // should be true
n1.greater(n3) // should be false