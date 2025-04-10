/*
Standard lists in Scala
this is the infix notation '::' this is a sort of cons(1,cons(2,...))
 */

val l: List[Int] = 1 :: 2 :: 3 :: Nil

// or val l = List(1,2,3)

l.head // returns the first element
l.tail // returns last element of the list

def sum(l: List[Int]): Int = {
  l match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }
}

def prod(l : List[Int]) : Int = {
  l match {
    case Nil => 1
    case x :: xs => x * prod(xs)
  }
}

/*
How can we generalize this code?
 */

/*
this is actually a right fold
 */
def fold1(acc: Int)( op: (Int, Int) => Int)(l: List[Int]) : Int = {

  def loop(l_loop: List[Int]) : Int = {
    l_loop match {
      case Nil => acc
      case x :: xs => op(x, loop(xs))
    }
  }

  loop(l)

}



//Only for some lambda does it work, first underscore is first parameter and second _ is second parameter
// val closure_sum = fold1(0)((_,_) => _ + _)
val closure_sum = fold1(0)((x,y) => x + y)
closure_sum(List(1,2,3,4,5))


/*
Making fold tail-recursive (this is now a left fold)

 */

def tailFold(acc: Int)(op: (Int, Int) => Int)(l: List[Int]): Int = {
  l match {
    case Nil => acc
    case x :: xs => tailFold(op(acc, x))(op)(xs)
  }

}

/*
If we have op(x,y) => y::x , this will create a list with y as its new head and x as its tail or the rest of the list

 */

def fold3[T](acc: T)(op: (T, Int) => T)(l: List[Int]): T = {

  l match {
    case Nil => acc
    case x :: xs => fold3(op(acc, x))(op)(l)
  }
}

val reverse = fold3(Nil)((x: List[Int], y : Int) => y::x)


//reverse(List(1,2,3))

List(1,2,3).foldRight(Nil: List[Int])(_ :: _) // return the same list as before

List(1,2,3).foldLeft(Nil: List[Int])((l: List[Int], head: Int) => head :: l) // return the reversed list

List(1,2,3).map(_+2) // returns a list of the same elements but with 2 added to them

List(1,2,3).filter(_%2==0) // returns divisors of 2 only from the list 1,2,3

List(1,2,3).zip(List(5,6,7)) // if theres more elements on one side, it will ignore

/*
Implementation of map


 */


def map[A,B](f: A=> B)(l: List[A]): List[B] = l.foldRight(Nil: List[B])( (x, acc) => f(x) :: acc )

