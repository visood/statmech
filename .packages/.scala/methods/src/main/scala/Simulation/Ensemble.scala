package Simulation


trait Ensemble[T]{ //finite or inifinite
  //lets use Option to classify an Ensemble as empty or not
  val LargeNumber: Int
  def sample: Option[T] //this should be a function, the ensemble is to be sampled randomly
  val weight: Option[Double] //None will mean that this Ensemble is empty


  val ranGen: util.Random
  val stats: Statistics[T]

  def join(that: Ensemble[T]): Ensemble[T] = (this.weight, that.weight) match {
    case (None, None) => ensemble( () => None, None)(ranGen, stats) //this could cause type parameterization problems
    case (None, Some(w)) => that
    case (Some(w), None) => this
    case (Some(v), Some(w)) => ensemble(
      () => if (ranGen.nextDouble() < v/(v+w)) this.sample else that.sample,
      Some(v + w)
    )(ranGen, stats)
  }

  def map[S](f: T => S)(implicit statsS: Statistics[S]): Ensemble[S] =
    ensemble( () => sample map f, weight)(ranGen, statsS)

  def flatMap[S](f: T => Ensemble[S])(implicit statsS: Statistics[S]): Ensemble[S] = {

    val fs = () => for (s <- sample;
                        r <- f(s).sample) yield r // sample flatMap {f(_).sample}

    val w = (
      for ( i <- 0 to LargeNumber;
                   w <- weight; s <- sample;
                   u <- f(s).weight) yield w*u
    ).sum/LargeNumber.toDouble

    ensemble(fs, if(w > 0.0) Some(w) else None)(ranGen, statsS)
  }

  def finiteMean(N: Int): T = stats.mean(for( i <- 0 to N; s <- sample) yield s)

  def finiteStdDev(N: Int): T = stats.stddev( for(i <- 0 to N;
                                                  s <- sample) yield s)

  lazy val mean: T = finiteMean(LargeNumber)
  lazy val stddev: T = finiteStdDev(LargeNumber)

}

object ensemble{
  def apply[T](f: () => Option[T], w: Option[Double])(implicit rg: util.Random,
                                                      st: Statistics[T]) =
    new Ensemble[T] {
      def sample = f()
      val weight = w
      val LargeNumber = 1000000
      val ranGen = rg
      val stats = st
    }

  def apply[T](f: () => Option[T],
               w: Double)(implicit rg: util.Random,
                          st: Statistics[T]): Ensemble[T] =
    apply( f, if (w > 0.0) Some(w) else None)
  
  def apply[T]( f: () => Option[T])(implicit rg: util.Random,
                                    st: Statistics[T]): Ensemble[T] =
    apply(f, 1.0)

  import collection.LinearSeq
  def apply[T](xs: LinearSeq[T],
               l: Int)(implicit rg: util.Random,
                       st: Statistics[T]): Ensemble[T] =
   apply( () => if (l==0) None else Some(xs(rg.nextInt(l))), l.toDouble)

  def apply[T](xs: List[T])(implicit rg: util.Random,
                            st: Statistics[T]): Ensemble[T] =
    apply( xs, xs.length)

  def collect[T](n: Int)(e: Ensemble[T]): Seq[T] =
    for( i <- 0 to n; s <- e.sample) yield s

  def weightOf[T](e: Ensemble[T]): Double = e.weight match{
   case None => 0.0
   case Some(w) => w
  }
}
  	
