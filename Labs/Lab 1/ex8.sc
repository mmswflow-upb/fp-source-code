def improve(xn: Double, a: Double): Double = (xn +  a/xn)/2


def nth_guess(n: Int, a: Double): Double = {

  def nth_guess_helper(i: Int, xn: Double): Double = {
    if(i == 0) xn
    else nth_guess_helper(i-1, improve(xn, a))
  }

  nth_guess_helper(n, 1.0)
}

nth_guess(10, 2.0)



