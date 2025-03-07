def sumNats(start: Int, stop: Int): Int = {
  if(start == stop) start
  else start + sumNats(start+1, stop)
}


def tailSumNats(start: Int, stop: Int): Int = {

  def sumNatsRec(i : Int, acc: Int): Int = {
    if(i == 0) acc
    else sumNatsRec(i-1, acc+i)
  }
  sumNatsRec(stop, 0)
}



sumNats(1,5)
tailSumNats(1,5)