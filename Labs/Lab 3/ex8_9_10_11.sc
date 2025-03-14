//Exercises 8 and 9

type Line2D = Int => Int
def translateOx(offset: Int)(l: Line2D): Line2D = (x: Int) => l(x-offset)
def translateOy(offset: Int)(l: Line2D): Line2D = (x: Int) => l(x) + offset

val l1: Line2D = (x: Int) => x
val l2: Line2D = (x: Int) => x + 1

val l1t: Line2D = translateOx(1)(l1) // this becomes l1t(x) = x - 1
val l2t: Line2D = translateOy(1)(l1) // this becomes l2t(x) = x + 1

// exercise 10

def intersect(l1: Line2D, l2: Line2D)(start: Int, stop: Int): Boolean = {
  def tailIntersect(i: Int): Boolean = {
    if(i > stop) false
    else if(l1(i) == l2(i)) true
    else tailIntersect(i+1)
  }
  tailIntersect(start)
}

val linesIntersect = intersect(l1, l2)(0, 10) // should be false (x + 1 != x)

// exercise 11

def larger(l1: Line2D, l2: Line2D)(start:Int, stop: Int) : Boolean = {

}

val l1Larger = larger(l1, l2)(0, 10) // should be false (x < x + 1)