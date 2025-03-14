def foldConditional(b: Int)(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {

  def tailFoldConditional(crt: Int, acc: Int): Int = {
    if (crt > stop) acc
    else {
      if(p(crt)) tailFoldConditional(crt + 1, op(crt, acc))
      else tailFoldConditional(crt + 1, acc) // skip to next if condition is not met on crt
    }
  }
  tailFoldConditional(start, b)
}


def sumOp(a: Int, b: Int): Int = a + b

def isEven(a: Int): Boolean = a % 2 == 0
def isOdd(a: Int): Boolean = a % 2 != 0

val sumOpResult = foldConditional(0)(sumOp, isEven)(1, 3) // result should be 2
val sumOpResult2 = foldConditional(0)(sumOp, isOdd)(1, 3) // result should be 4