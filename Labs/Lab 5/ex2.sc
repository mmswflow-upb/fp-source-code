trait OList{
  def head: Int
  def tail: OList
  def foldRight[B](acc: B)(op: (Int,B) => B): B // B is a generic type, it can be any type of value like pairs (Int, Int) or even OList
  def foldLeft[B](acc: B)(op: (B,Int) => B): B
  def indexOf(i: Int): Int
  def filter(p: Int => Boolean): OList
  def map(f: Int => Int): OList
  def partition(p: Int => Boolean): (OList, OList)
  def slice(start: Int, stop: Int): OList
  def forall(p: Int => Boolean): Boolean
}

case object Void extends OList {
  override def head: Int = Int.MinValue
  override def tail: OList = Void
  override def foldRight[B](acc: B)(op: (Int,B) => B): B = acc
  def foldLeft[B](acc: B)(op: (B,Int) => B): B = acc
  def indexOf(i: Int): Int = -1
  def filter(p: Int => Boolean): OList = Void
  def map(f: Int => Int): OList = Void
  def partition(p: Int => Boolean): (OList, OList) = (Void,Void)
  def slice(start: Int, stop: Int): OList = Void
  def forall(p: Int => Boolean): Boolean = false
}

case class Cons(x: Int, xs: OList)  extends OList{
  override def head: Int = x
  override def tail: OList = xs

  /*
  Go to the end of the list (while leaving accumulator unchanged), then start doing the operation (op) between the accumulator and current head, then
  due to recursive calls, the result of that operation will be returned into another operation,
  so in the end you get:  x1 op (x2 op (x3 op acc) )
  */

  override def foldRight[B](acc: B)(op: (Int,B) => B): B = op(x, xs.foldRight(acc)(op))


  /*
  Iterate through elements one by one, compute the operation between the accumulator and the current head, then go on to next element
  by calling leftFold recursively, so you will get  ((acc op x0) op x1) ...
 */

  override def foldLeft[B](acc: B)(op: (B,Int) => B): B = xs.foldLeft(op(acc, x))(op)

  /*
  The accumulator is the new filtered list, the operation checks if the current "head" satisfies the condition "p", if it does then we construct
  a new list with the new head on its left, if not we leave it as it is. We start with Void to indicate the empty list. We use foldRight
  because we want to go from right to left to add our filtered elements in order while constructing this new list
   */
  override def filter(p: Int => Boolean): OList = this.foldRight[OList](Void)(( head: Int, accL: OList) => if(p(head)) Cons(head, accL) else accL )

  /*
  We use fold left to go through each element from left to right. Our accumulator is a pair of indices, the first one is the index
  at which the element we're looking for is at, the second index is the current index. On each iteration we increment the second index unconditionally, but the first index
  changes only when we find the desired element in the list, thus the first index's value will be equal to the current index at that point
  */

  override def indexOf(i: Int): Int =
    this.foldLeft[(Int, Int)]((-1, -1))(
      (pairOfIndices: (Int, Int), head: Int ) =>
        if(head == i) (pairOfIndices._2 + 1, pairOfIndices._2 + 1) else (pairOfIndices._1, pairOfIndices._2 + 1)
    )._1


  /*
  Just like filter, but instead of picking elements which we want to put into the new list based on a predicate, we apply
  the same function on all the elements of the list and put the results into a new list
   */
  override def map(f: Int => Int): OList = this.foldRight[OList](Void)( (head: Int, accL: OList) => Cons( f(head) ,accL) )

  /*
  Just like in filter (again) because we want to reconstruct the lists in the original order, we use foldRight. For this to work we create
  an accumulator of type (OList, OList), so a pair of lists, the left list has elements that don't satisfy the predicate "p", the right list
  has the elements that satisfy it. For iteration through the list, we check using the operation whether the current "head" satisfies the predicate,
  if it does then we create a new list with Cons, and the first element in it will be the head, the rest of the list is the right list, since it had the
  elements that satisfy the predicate. If the current element "head" doesn't satisfy the predicate, then we construct a new list with its first element being
  the current element "head" and the rest of the list is what we had before as the left list (elements that don't satisfy the predicate)
   */
  override def partition(p: Int => Boolean): (OList, OList) = foldRight[(OList, OList)]((Void, Void)){
    case (head, (leftL, rightL)) => if(p(head)) (leftL, Cons(head, rightL)) else  (Cons(head, leftL), rightL)
  }


  /*
    Returns the list of elements found in the range of indices defined by start and stop

    First we compute the size of the list. Then we apply foldRight to construct the new sliced array in the correct order, we iterate
    through the list backwards, on each iteration we decrement the current index which started at size -1, in order to check if the current
  element must be added in the sliced list or not, so we compare the current index with the start and stop to see if it's in the range

  */
  override def slice(start: Int, stop: Int): OList = {

    val size = foldLeft[Int](0)((size: Int, _) => size+1)

    foldRight[(OList, Int)]((Void, size-1)){
      case (head, (resL, currIndx)) => if(currIndx >= start && currIndx <= stop) (Cons(head, resL), currIndx - 1) else (resL, currIndx - 1)
    }._1
  }

  /*
  Go from left to right through each element, the starting (accumulator) value is 1, if one of the elements satisfies the predicate (p), then
  we leave accumulator as it is, if not then we set our accumulator to zero. At the end, if the result is zero then not all elements satisfied
  the condition so it returns false, otherwise it's true
  */
  override def forall(p: Int => Boolean): Boolean = this.foldLeft[Int](1)((acc: Int, head: Int) => if(p(head)) acc else 0) == 1
}
