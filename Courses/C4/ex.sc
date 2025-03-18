/*
Plan

1- The structure of the type
2- The type operations and how they behave
3- properties of the type
 */

/* The type Nat: (Natural numbers)

1- Constructors for Nat: We need a representation for zero:

First constructor: Zero: Nat
Second constructor: Succ : Nat -> Nat (from successor)

So a sequence of natural numbers would be:

Zero, Succ(Zero), Succ(Succ(Zero)), Succ(Succ(Succ(Zero))), ....

this is how we decided to represent natural numbers, by applying the constructor again and
again over the previous numbers

All possible values of type Nat should be constructable through combining the two constructors

2- Operations for Nat:

- isZero : Nat -> Boolean
-
ex: isZero(Zero) = true ; isZero(succ(n)) = false , n being any variable of type Nat , these are axioms, they dont need to be proven, they're just "rules"
the axioms are independent of implementations and any implementation that satisfies the axioms will be correct

The addition:

add: Nat x Nat -> Nat
add(n, m) = n times Succ applied on m

add(Succ(mp), m) = add(mp, Succ(m))

add(Zero, m) = add(m, Zero) = m

greater: Not x Not -> Boolean

greater(succ(n), zero) = true ; here n can be anything too
greater(zero, n)  = false, n can be anything even zero and it still holds true
greater(succ(m), succ(n)) = greater(m, n) = true, so here we unfolded the "covers" recursively, until we get one of
the above cases
*/
//Specify type, traits are similar to interfaces from java


/*
The differences between case classes in scala and normal classes:

1. Instances of the casae classes are immutable (not changeable)
2. Members of the case classes are also immutable, for example the parameters of Succ 'n' is immutable
3. Case classes have only one constructor
4. Comparison is done structurally, not referentially
5. Match, it can only recognize patterns built with type constructors


so this doesnt work:

3 match {
  case 1 + 2 => true
}

* */

trait Nat

// We notice that the axioms we've written above are overlapping with what we're writing below as implementation
case object Zero extends Nat // This is a singleton (one object referenced by multiple vars could be possible)

/*
in java: (whats equivalent)

public class Succ extends Nat {

  public Nat n;
  public Succ(Nat n) {
    this.n = n;
  }
}
 */
case class Succ(n: Nat) extends Nat



val z1 = Zero
val z2 = Zero

z1 == z2

val x: Nat = Succ(Succ(Zero))
val y: Nat = Succ(Succ(Zero))

x == y // This is true because they're structurally the same, they're constructed in the same way, equals method is implemented directly by Scala

def isZero(n: Nat) : Boolean = {

  // it will compare n with the structures mentioned below, go from most specific to most general
  n match {
    case Zero => true // patterns
    case Succ(_) => false // we dont even need to mention n, any number there
  }
}

def add(n: Nat, m: Nat): Nat = {

  n match {
    case Zero => m
    case Succ(np) => add(np, Succ(m))
  }

}

def largerThanThree(n: Nat) : Boolean = {
  n match {
    case Succ(Succ(Succ(Succ(_)))) => true
    case _ => false
  }
}

largerThanThree(Succ(Succ(Succ(Succ(Succ(Zero)))))) // it just finds that pattern even if there's more than 4 succ's, as long as 4 succs are found its greater than 3

/*
Scala supports pairs


 */


def greater(n: Nat, m: Nat) : Boolean = {
  //We can match pairs
  (n,m) match {
    case (Zero, _) => false
    case (Succ(np), Succ(mp)) => greater(np, mp) // this can be switched with the one below it, they're complementary in this case
    case (Succ(_), Zero) => true // this can be replaced with whats below because Zero as a first element in the pair is already covered so the underscore wouldnt be zero due to order
    //    case (_, Zero) => true
  }
}


/*
Lets implement a very simple list of integers:
 */

trait IList

case object Void extends IList
case class Cons(x: Int, xs: IList) extends IList

val l1 = Cons(1, Cons(2, Cons(3, Void)))

// x and xs are the head and tail of the list, void signifies where to stop or if it's empty

def size(l: IList) : Int = {
  l match {
    case Void => 0
    case Cons(_, xs) => size(xs) + 1
  }
}

def append(l1: IList, l2: IList) : IList = {

  l1 match {
    case Void => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

}