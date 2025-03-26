/*
Decomposition refers to how we manage the inner structure of an object

In Scala, functional decomposition uses 'match' to 'decompose' or look inside to examine the structure of an object


 */


/*
Operations are organized inside this Nat class
 */
trait Nat {
  def isZero: Boolean
  def add(other: Nat) : Nat
  def equals(other: Nat) : Boolean
  def greater(other: Nat): Boolean
}

/*
we need to implement the operations of the class
 */


case object Zero extends Nat {

  override def isZero: Boolean = true
  override def add(other: Nat) = other
  override def equals(other: Nat) : Boolean = {
    other match {
      case Zero => true
      case Succ(_) => false
    }
  }
  override def greater(other: Nat) : Boolean = false
}

case class Succ(n: Nat) extends Nat  {

  override def isZero: Boolean = false
  override def add(other: Nat): Nat = Succ(n.add(other))
  override def equals(other: Nat): Boolean = {
    other match {
      case Zero => false
      case Succ(m) => n.equals(m)
    }
  }
  override def greater(other: Nat) : Boolean = {
    other match {
      case Zero => true
      case Succ(m) => n.greater(m)
    }
  }

}



case object Infinity extends Nat {
  override def isZero: Boolean = false
  override def add(other: Nat) : Nat = Infinity
  override def equals(other: Nat) : Boolean = {
    other match {
      case Infinity => true
      case _ => false
    }
  }
  override def greater(other: Nat) : Boolean = {
    other match {
      case Infinity => false
      case _ => true
    }

  }
}


/*
how do we decide one implementation over the other

suppose we want to introduce a new "kind" of Nat. A special value - infinity. What does this entail?

When introducing new kind of values (new case classes or objects), its easier in OOP to implement them because in the
functional approach it's inconvenient to go over the whole implementation and accommodate the new types, as for OOP
we can just implement the operations differently for each new type


But when adding new operations, it's easier in functional programming than in OOP, because we have only one whole implementation
of the operations, but in OOP it's harder because we have to add the new operation in each implementation of each case class or case object

 Most Scala collections are implemented using OO style
 */