package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val av = a()
    val bv = b()
    val cv = c()
    pow(bv,2) - (4*av*cv)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val dv = delta()
    if(dv < 0) Set()
    else{
      val sol1 = (-1*b() + sqrt(dv)) / 2*a()
      val sol2 = (-1*b() - sqrt(dv)) / 2*a()
      Set(sol1, sol2)
    }

  }
}
