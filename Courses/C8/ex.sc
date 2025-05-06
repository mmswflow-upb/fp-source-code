/*
Polymorphism: Multiple shapes, shapes can refer to objects and classes, functions (overloading, overriding)

1.Inheritance is an example of subtype polymorphism, if A is a specialization of B, then any property of entities of
type B, should carry to objects of type A

for example: A inherits B

if "o" is part of B and P(o) the
n also any object of type A has the same property

2.Overloading: it's an implementation strategy of  (Ad-hoc polymorphism: one name, multiple implementations)

3.Overriding: implementing the same function differently in different classes ( not really polymorphism)

4.Parametric Polymorphism: "one name, one implementation"

example:

We define a function size for lists, we don't care about the type of lists we're working with.
the contained type of the list can be arbitrary (like generics) like:

def size[A](l: List[A]): Int=...

 */


/*
How do we deal with errors in Scala?

  try-catch blocks are deprecated in scala
 */

trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

//How do we deal with negative integers

//trait Maybe
//case object Error extends Maybe
//case class Value(x: Nat) extends Maybe



//def fromInteger(i: Int) : Maybe = {
//
//  def convert(x: Int): Nat = {
//    if(x == 0) Zero
//    else Succ(convert(x-1))
//  }
//
//  if(i < 0) Error
//  else Value(convert(i))
//}
//
//fromInteger(10).match {
//  case Error => ???
//  case Value(x) => ???
//}

/*
Now we need to generalize the Value() type
Scala has already defined this and its called Option
 */

def fromInteger(i: Int): Option[Nat] = {

  def convert(x: Int): Nat = {
    if (x == 0) Zero
    else Succ(convert(x - 1))
  }

  if (i < 0) None
  else Some(convert(i))
}

fromInteger(10).match {
  case None => "Failed"
  case Some(x) => "Succeeded"
}



/*
When creating a list of these two, we need to create a list of an upper class to include both Dog and Cat,
so Maybe[Dog] is a subtype of Maybe[Animal] and Maybe[Cat] is a subtype of Maybe[Animal], is this actually true? Yes, in Scala it's called Type Covariance

We would like that Maybe is covariant in its type parameter "A"

example: T2 is subtype of T1, then Maybe[T2] is subtype of Maybe[T1]

 How do we make a class covariant? we add a plus before the type parameter, for our example, since Maybe is covariant, then Maybe[???] should be a subtype of anything
 and the only type that is the subtype of anything in Scala is called "Nothing"

 There's also the reverse of that, contravariant, where Maybe[T1] is a subtype of Maybe[T2],
 usually when we want to serialize stuff we do this

Lists are also polymorphic, trees, maps etc and they're covariant

 This means that the implementation of the list is:

 trait List[A]
 case object Void extends List[Nothing]
 case class Cons[+A](x: A, xs: List[A]) extends List[A]
 */

trait Maybe[+A]
case object Error extends Maybe[Nothing]
case class Value[+A](x: A) extends Maybe[A]


def size[A](l: List[A]): Int = {
  l match {
    case Nil => 0
    case _ :: xs => 1 + size(xs)
  }
}

trait Animal {
  def howl: String = "!"
}

class Dog extends Animal  {

  override def howl: String = "barf"
}

class Cat extends Animal {
  override def howl: String = "meow"
}


val l = List(Value(new Dog), Value(new Cat))

val dog: Maybe[Dog] = Value(new Dog)
val cat: Maybe[Cat] = Value(new Cat)



def fromInteger2(i: Int): Maybe[Nat] = {

  def convert(x: Int): Nat = {
    if (x == 0) Zero
    else Succ(convert(x - 1))
  }

  if (i < 0) Error
  else Value(convert(i))
}

fromInteger2(10).match {
  case Error => "Failed"
  case  Value(x) => "Succeeded"
}