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

/*

Go through the list from the beginning till the nth element, then return recursively to the front again, thus building the
resulting array in the same order as the one given, we have to go n times

*/

def take(n: Int)(l: IList): IList = {
  l match {
    case Void => Void
    case Cons(x, xs) => if(n == 1)  Cons(x, Void) else Cons(x, take(n-1)(xs)) // till n == 1 because at the end we include x and start returning elements in the correct order
  }
}



val testList2 = Cons(1, Cons(2, Cons(3, Cons(10, Cons(5,Void)))))
val takeTestTwo = take(2)
takeTestTwo(testList2)

/*
While n is still not zero keep going recursively
 */
def drop(n: Int)(l: IList): IList = {

  l match {
    case Void => Void
    case Cons(x, xs) => if(n==0) l else drop(n-1)(xs)
  }
}


val dropTestTwo = drop(2)

dropTestTwo(Void)
dropTestTwo(Cons(1,Void))
dropTestTwo(Cons(1, Cons(2,Void)))

/*
Append l2 to l1, so move elements from l2 one by one to l1
 */

//def append(l1: IList, l2: IList): IList = {
//  l1 match {
//    case Void => l2
//    case Cons(x, xs) =>
//  }
//}


def last(l: IList): Int = {
  l match {
    case Void => Int.MinValue
    case Cons(x, Void) => x
    case Cons(x, xs) => last(xs)
  }
}

last(testList2)


def isSorted(l: IList): Boolean = {
  def isSortedHelper(prev: Int ,remL: IList): Boolean  = {
    remL match {
      case Void => true
      case Cons(x, xs) => if(prev > x) false else isSortedHelper(x, xs)
    }
  }
  isSortedHelper(Int.MinValue,l)

}

isSorted(testList2)



