import scala.math._


def mySqrt(a: Double): Double = {
  def improve(xn: Double): Double = (xn +  a/xn)/2
  def acceptable(xn: Double): Boolean = math.abs(xn * xn - a)/a <= 0.001

  def tailSqrt(estimate: Double): Double = {
    if (acceptable(estimate)) estimate
    else tailSqrt(improve(estimate))
  }

  tailSqrt(1.0)
}

mySqrt(3.0)