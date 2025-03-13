def subtractRange(x: Int, start: Int, stop: Int): Int = {
  if(start == stop) x-start
  else subtractRange(x-start, start+1, stop)
}

subtractRange(10, 1, 3)


