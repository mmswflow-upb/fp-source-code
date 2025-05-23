object FSets {

  type Set = Int => Boolean

  def tokenID: Int = 1

  def member(e: Int)(s: Set): Boolean  = s(e)

  // This defines a set of one element, this characteristic function will check if any given element matches the singleton's sole element
  def singleton(x: Int): Set = (y: Int) => x == y


  def ins(x: Int)(s: Set): Set = (y: Int) => singleton(x)(y) || s(y)

  def fromBounds(start: Int, stop: Int): Set = (y:Int) => {
    /*

    Non-tail recursive version:

    if(start > stop)  false
    else  fromBounds(start+1, stop)(y) || singleton(start)(y)

    */
    def helperFromBounds(i: Int, traversedSet: Set) : Boolean = {
      if(i > stop) traversedSet(y)
      else helperFromBounds(i+1, ins(i)(traversedSet))
    }
    helperFromBounds(start, y => false)
  }

  def union (s1: Set, s2: Set): Set = (x: Int) => s1(x) || s2(x)

  def complement(s1: Set): Set = (y: Int) => !s1(y)

  def sumSet(b: Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = {
      if(crt > stop) acc
      else if(s(crt)) aux(crt+1, acc + crt ) else aux(crt+1, acc)
    }

    aux(start, b)
  }

  def foldLeftSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = {
      if(crt > stop) acc
      else if(s(crt)) aux(crt+1, op(acc, crt)) else aux(crt+1, acc)
    }
    aux(start,b)
  }

  def foldRightSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    if (start > stop) b
    else if(start == stop) if(s(start)) op(start, b) else b
    else if(s(start)) op(start, foldRightSet(b)(op)(start+1, stop)(s)) else foldRightSet(b)(op)(start+1, stop)(s)

  }

  def filter(p: Int => Boolean)(s: Set): Set = (y: Int) => s(y) && p(y)

  def partition(p: Int => Boolean)(s: Set): (Set,Set) =  ( (y: Int) => s(y) && p(y), (y: Int) => s(y) && !p(y))

  def forall(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    if(start > stop) true
    else if(s(start) && !cond(start)) false
    else forall(cond)(start+1,stop)(s)
  }


  def exists(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    if(start > stop) false
    else if(s(start) && cond(start)) true
    else exists(cond)(start+1, stop)(s)
  }

  def setOfDivByK(k: Int): Set = filter((x: Int) => x % k == 0)(_ => true)

  def moreDivs(k: Int)(start: Int, stop:Int)(s1: Set, s2: Set): Boolean = {

    val increment = (x: Int, y: Int) => x + 1
    val numOfDivs = (s: Set) => foldLeftSet(0)(increment)(start,stop)(filter(setOfDivByK(k))(s))
     numOfDivs(s1) > numOfDivs(s2)
  }

}
