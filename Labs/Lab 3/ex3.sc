def foldRight (b: Int)(op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  def tailFoldRight(crt: Int, acc: Int): Int  = {
    if (crt < start) acc
    else tailFoldRight(crt-1, op(crt, acc)) // performs operations with the 'stop' value as our first crt
  }5
  /*
    this isn't the final returned value, because we still
    have the (start, stop) pair of parameters to pass to the foldRight function in order
    to call tailFoldRight for the first time
   */
  tailFoldRight(stop, b)
}

// testing it with a sub operation
def subOp (a: Int, b: Int): Int = a - b

//1 - ( 2 - (3 - (4 - 0))) = -2
val subResult = foldRight(0)(subOp)(1,4)

