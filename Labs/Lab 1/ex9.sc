import scala.math._


def acceptable(xn: Double, a: Double): Boolean = {
  math.abs(xn * xn - a) <= 0.001
}


