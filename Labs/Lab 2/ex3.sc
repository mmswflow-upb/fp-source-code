def sumSquares(n: Int): Int = {
  if(n == 0) 0
  else n*n + sumSquares(n-1)
}

// tail recursive version

def tailSumSquares(n: Int): Int = {
  def sumSquaresRec(i: Int, acc: Int): Int = {
    if(i == 0) acc
    else sumSquaresRec(i-1, acc + i*i)
  }
  sumSquaresRec(n, 0)
}

sumSquares(3)
tailSumSquares(3)
