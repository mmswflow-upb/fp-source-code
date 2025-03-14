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


// Exercise 5

def sumSquares(n: Int) : Int = foldMap(sum, square)(1, n)

val squaresSum_v2 = sumSquares(3) // should be the same as squaresSum

// Exercise 6

def isDivisible(x: Int, k : Int) : Int = if (x % k == 0) 1 else 0

/*
the function f must take only one argument which is the current ith number in the range,
that's why we need to use a lambda function to pass the x value to isDivisible along with the k value
this could be done directly with only one lambda function
 */

def hasDivisor(k: Int, start : Int, stop: Int) : Boolean = foldMap(sum, (x : Int) => isDivisible(x, k))(start, stop) >= 1

val test1 = hasDivisor(9, 1, 27) // 9, 18, 27 are divisible by 9