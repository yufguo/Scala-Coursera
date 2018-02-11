package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val t = b()
      t*t - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val av = a()
      val bv = b()
      def cal(n: Double): Double = (-bv + n)/(2*av)
      val t = delta()
      if(t < 0) Set()
      else if(t == 0) Set(cal(t))
      else Set(cal(-t), cal(t))
    }
  }
}
