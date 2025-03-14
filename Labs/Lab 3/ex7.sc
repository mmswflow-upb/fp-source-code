/*
What I'm doing here is basically first checking if the current interval is smaller than e, if it is then I return the area of the trapezoid,
if it is not then I split the interval in half and call the function recursively on the two halves. This continues until the interval is smaller than e.
This is a recursive function, but it is not tail recursive because the recursive call is not the last thing that happens in the function.
The last thing that happens is the addition of the two results of the recursive calls.
 */

def integrate(f: Double => Double)(start: Double, stop: Double)(e: Double): Double = {

  def helperIntegrate(tailStart: Double, tailStop: Double ) : Double = {

    if ((tailStop - tailStart) <= e) (f(tailStart) + f(tailStop)) * (tailStop - tailStart) / 2
    else helperIntegrate(tailStart, (tailStop  + tailStart)  / 2) + helperIntegrate( (tailStart + tailStop)  / 2, tailStop) // this is not tail recursive anymore!
  }
  helperIntegrate(start, stop)
}


val f = (x: Double) => x * x
val start = 0.0
val stop = 10.0
val e =  1.0 // 1.0 is the smallest interval we can have
val result = integrate(f)(start, stop)(e)

