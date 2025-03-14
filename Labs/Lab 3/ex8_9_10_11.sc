type Line2D = Int => Int
def translateOx(offset: Int)(l: Line2D): Line2D = (x: Int) => l(x-offset)
def translateOy(offset: Int)(l: Line2D): Line2D = (x: Int) => l(x) + offset

val l1: Line2D = (x: Int) => x

val l1t: Line2D = translateOx(1)(l1) // this becomes l1t(x) = x - 1
val l2t: Line2D = translateOy(1)(l1) // this becomes l2t(x) = x + 1

