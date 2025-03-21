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

val listContainsOne = contains(1, testList)


def max(l: IList): Int = {
  l match {
    case Void => Int.MinValue
    case Cons(x, xs) => Math.max(x,max(xs))
  }
}

val maxValOfList = max(testList)

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
val resultTakeTwo = takeTestTwo(testList2)

/*
While n is still not zero keep going recursively dropping the first elements one by one and return the rest of the list when n reaches zero
 */
def drop(n: Int)(l: IList): IList = {

  l match {
    case Void => Void
    case Cons(x, xs) => if(n==0) l else drop(n-1)(xs)
  }
}


val dropTestTwo = drop(2)

val dropTestOnVoid = dropTestTwo(Void)
val dropTestOn1ElemList = dropTestTwo(Cons(1,Void))
val dropTestOn2ElemList = dropTestTwo(Cons(1, Cons(2,Void)))

/*
Append l2 to l1, so move elements from l1 one by one to new list
then add all l2 as the rest of the list so we dont have to add its elements
one by one anymore
 */

def append(l1: IList, l2: IList): IList = {
  l1 match {
    case Void => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }
}

/*
l1 = 1 2 3 Void
l2 = 4 5 6 Void

l1 ->  1 => Cons(1, append(xs, l2))
->
 */


val appendLists = append(testList, testList2)

def last(l: IList): Int = {
  l match {
    case Void => Int.MinValue
    case Cons(x, Void) => x
    case Cons(x, xs) => last(xs)
  }
}

val lastElemOfList2 = last(testList2)

/*
We recursively iterate through the list left to right till we reach the
end or 'Void', then we return a new list with the last elements put in first
and we do this by appending the elements we first went through to the list of
elements that were last
*/

def reverse(l : IList) : IList = {

  l match {
    case Void => Void
    case Cons(x, xs) => append(reverse(xs), Cons(x, Void))
  }
}

/*
l = 1 2 3 Void

1 -> append(reverse(2, 3 , Void) ,
 */

/*
Consume 'l' from left to right, and each time we consume an element we add
it to the accumulator 'accL' as the newest element, so the actual first elements
are already stored in accL and they're on the right side now, which basically means
that we reversed the list
 */

def tailReverse(l : IList ,accL : IList = Void): IList = {

  l match {
    case Void => accL
    case Cons(x, xs) =>  tailReverse(xs ,Cons(x, accL))
  }
}

val reverseResult = reverse(testList2)
val tailReverseResult = tailReverse(testList2)

def isSorted(l: IList): Boolean = {
  def isSortedHelper(prev: Int ,remL: IList): Boolean  = {
    remL match {
      case Void => true
      case Cons(x, xs) => if(prev > x) false else isSortedHelper(x, xs)
    }
  }
  isSortedHelper(Int.MinValue,l)

}

val isList2Sorted = isSorted(testList2)

/*
 This works as if we're using indices i and j for l1 and l2
 we consume the lists (increasing the index somehow) and we check for
 the first number of l1 and first number of l2, if l2[i] is smaller, we consume
l2 so we add its first element (append it) to the accumulator and we keep l1 the same
(pass it on exactly the same in the next iteration)
if l1[j] is bigger then we do the exact same thing but in reverse
now if  we reach one of the lists end (Void), we just append the other list to the accumulator
 */


def merge(l1: IList, l2: IList, accL: IList = Void): IList = {


  (l1, l2) match{
    case (Void, Void) => accL
    case (Void, l2) => append(accL, l2)
    case (l1, Void) => append(accL, l1)
    case (Cons(x, xs), Cons(y, ys)) => if(x >= y)  merge(l1, ys, append(accL, Cons(y, Void))) else merge(xs, l2, append(accL, Cons(x, Void)))
  }
}

val mergeResult = merge(Cons(1,Cons(3,Cons(5,Void))), Cons(0, Cons(2, Cons(4, Void))), Void)

/*
split array in half recursively, till the array length is 1, then start merging sub arrays with the function defined above,
so that elements will be placed in the correct order in the new sublist, because the sub arrays are considered to be sorted
we can make use of the 'merge' function, in order to split the list we need to use the take and drop functions
*/

def mergeSort(l: IList) : IList = {

  val sizeL = size(l)

  l match {
    case Cons(x, Void) => Cons(x, Void)
    case Cons(x, ys) => merge(mergeSort(take(sizeL/2)(l)) , mergeSort(drop(sizeL/2)(l))) // split the array in two with drop and take, apply mergeSort on both then merge
  }
}



val sortedList = mergeSort(Cons(10,Cons(2,Cons(-3,Cons(5,Cons(0,Cons(9,Void)))))))
