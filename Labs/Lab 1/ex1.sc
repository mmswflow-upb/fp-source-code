def fact (n: Int): Int = {
  def aux_fact(i: Int, acc: Int): Int =
    if (i <= 1) acc
    else aux_fact(i-1, acc * i)

  aux_fact(n, 1)
}

fact(3)

