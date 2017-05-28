package Simulation

 
//a type-class for statistics on type T

trait Statistics[T]{ //mean, and variance of things of type T should itself be of type T
  def plus (a: T, b: T): T
  def minus (a: T, b: T): T
  def times(a: Double, b: T): T
  def divide(a: T, b: T): Double //scalable T
  def divide(a: T, l: Double): T

  def times(a: T, b: Double): T = times(b, a)
  def divide(a: T, l: Int): T = divide(a, l.toDouble)
  def sum(xs: Seq[T]): T = xs reduceLeft ( (x: T, y: T) => plus(x, y))
  def mean(xs: Seq[T]): T = divide(sum(xs), xs.length)
  def variance(xs: Seq[T]): Double = {
    val m = mean(xs)
    val vs = for (x <- xs) yield (divide(minus(x, m),m)*divide(minus(x, m),m)) 

    vs.sum/xs.length
  }
  def stddev(xs: Seq[T]): T = times(mean(xs), math.sqrt(variance(xs)))
}

object statisticsImplicits{
  implicit object statisticsDoubles extends Statistics[Double]{
    def plus(a: Double, b: Double) = a + b
    def minus(a: Double, b: Double) = a - b
    override def times(a: Double, b: Double) = a*b
    def divide(a: Double, b: Double) = a/b
  }
}

