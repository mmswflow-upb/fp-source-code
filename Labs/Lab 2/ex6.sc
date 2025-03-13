def subtractRange(x: Int, start: Int, stop: Int): Int = {
  if(start == stop) stop - x
  else start - subtractRange(x, start+1, stop)
}

subtractRange(100, 0, 10)

