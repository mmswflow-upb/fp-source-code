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
