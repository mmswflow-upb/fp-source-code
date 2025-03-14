def foldWith (b: Int)(op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    else tail_fold(crt + 1, op(crt, acc))
  }
  /*
    this isn't the final returned value, because we still
    have the (start, stop) pair of parameters to pass to the foldWith function in order
    to call tail_fold for the first time
   */
   tail_fold(start, b)
}

// testing it with a sum operation
def sumOp (a: Int, b: Int): Int = a + b
def multOp (a: Int, b: Int): Int = a * b


val sumOpResult = foldWith(0)(sumOp)(1,3)
val multOpResult = foldWith(1)(multOp)(1,3)
