def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  def tailFoldMap(i: Int, acc : Int): Int = {
    if(i > stop) acc
    else tailFoldMap(i+1, op(acc, f(i))) // apply function on ith number and accumulate it using the operation
  }
  tailFoldMap(start+1, f(start)) // start from the next number and apply the function on the start number
}

def square(x: Int) : Int  = x*x
def unit(x: Int) : Int = x
def sum(x: Int, y: Int) : Int = x + y
def sub(x: Int, y: Int) : Int = x - y

/*
  1^2 + 2^2 + 3^2 = 0 + 1 + 4 + 9 = 14
  1^2 - 2^2 - 3^2 = 0 - 1 - 4 - 9 = -12
 */

val squaresSum = foldMap(sum, square)(1,3)
val squaresSub = foldMap(sub, square)(1,3)

