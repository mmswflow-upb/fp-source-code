trait IList
case object Void extends IList
case class Cons(x: Int, xs: IList) extends IList


def isEmpty(l: IList) : Boolean = {
  l match {
    case Void => true
    case _ => false
  }
}

def size(l: IList): Int = {
  l match {
    case Void => 0
    case Cons(_, xs) => 1 + size(xs)
  }
}

def contains(e: Int, l: IList): Boolean = {
  l match {
    case Void => false
    case Cons(x, xs) => if (x == e) true else contains(e, xs)

  }
}

val testList = Cons(1, Cons(2, Cons(3, Void))) // [1, 2, 3]

contains(1, testList)


def max(l: IList): Int = {
  l match {
    case Void => Int.MinValue
    case Cons(x, xs) => Math.max(x,max(xs))
  }
}

max(testList)



