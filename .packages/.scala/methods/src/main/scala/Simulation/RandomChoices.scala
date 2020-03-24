package Simulation

import annotation.tailrec
import language.implicitConversions

//consider making this a library 
class RandomlyAccessible[T](val v: IndexedSeq[T]){
  def index(implicit rg: util.Random): Int = rg.nextInt(v.length)
  def chooseOne(implicit rg: util.Random): T = v( index )
  def pickWithReplacement(n: Int)(implicit rg: util.Random): IndexedSeq[T] = {
    require (n >= 0)
    if (n == 0) IndexedSeq() else chooseOne +: pickWithReplacement(n - 1)
  }

  @tailrec final def anotherGathered(n: Int,
                                     cs: IndexedSeq[T])(implicit rg: util.Random):
      IndexedSeq[T] =
  {
    if (n == 0) cs
    else {
      val i = index
      RandomlyAccessible(v.updated(i, v.last).init).
        anotherGathered(n - 1, cs :+ v(i))
    }
  }

  def pick(n: Int)(implicit rg: util.Random): IndexedSeq[T] =
    anotherGathered(n, IndexedSeq[T]())

/*
  def pick(n: Int)(implicit rg: util.Random): IndexedSeq[T] = {
    require ( (n >= 0) && (n <= v.length))
    var u = v
    for( i <- 0 until n) yield {
      val i = rg.nextInt(u.length)
      val ri = u(i)
      u = u.updated(i, u.last).init
      ri
    }
  }
*/
  
/*
  def pick(n: Int)(implicit rg: util.Random): IndexedSeq[T] = {
    @tailrec def acc(n: Int, cs: IndexedSeq[T], xs: IndexedSeq[T]): IndexedSeq[T] = {
      if (n == 0) cs
      else {
        val i = rg.nextInt(xs.length)
        acc(n-1, cs :+ xs(i), xs.updated(i, xs.last).init)
      }
    }
    acc(n, IndexedSeq[T](), v)
  }
*/

  def pair(implicit rg: util.Random): (T, T) = {
    val v = pick(2)
    (v(0), v(1))
  }

}

object RandomlyAccessible
{
  //any sequence can be made randomly accessible
  implicit def toRandomlyAccessible[T](v: IndexedSeq[T]) =
    new RandomlyAccessible(v)

  def apply[T](v: IndexedSeq[T]): RandomlyAccessible[T] =
    new RandomlyAccessible(v)

  def apply[T](s: Set[T]) : RandomlyAccessible[T] = apply(s.toIndexedSeq)

}


class RandomlyChosen(implicit val ranGen: util.Random){
		
  def uniform: Double = ranGen.nextDouble()
  def uniform[T](xs: IndexedSeq[T]): T = xs(ranGen.nextInt(xs.length))
  //another algorithm
  def withoutReplacement[T](xs: IndexedSeq[T], n: Int): Seq[T] = {
    val l = xs length
    var cmap = Map[T, T]()
    def point(x: T, m: Int) = {
      cmap += x -> xs(l - m)
      x
    }
    def instead(x: T, m: Int): T = if (cmap contains x) instead(cmap(x), m)
                                   else point(x, m)
    def chooseNext(m: Int): T =  instead(xs(ranGen.nextInt(l-m + 1)), m)

    (1 to n) map chooseNext
  }
    
  
  def pair[T]( xs : IndexedSeq[T]): (T, T) = {
    val l = xs.length
    val first = xs(ranGen.nextInt(xs.length))
  	//println ("chose" + first.toString)
    def second: T = xs(ranGen.nextInt(l - 1)) match{
      case `first` => xs(l - 1)
      case x => x
    }
    (first, second)
  }
  
  def butNot[T](xs: IndexedSeq[T])( y: T) : T =
    xs(ranGen.nextInt(xs.length)) match{
      case 'y' => butNot(xs)(y)
      case x => x
    }

  def chooseAndExclude[T](xs: IndexedSeq[T]): (T, IndexedSeq[T]) = {
    val i = ranGen.nextInt(xs.length)
    (xs(i), xs.updated(i, xs.last).init)
  }


}

object probops
{
  def normalized(ws: IndexedSeq[Double]): IndexedSeq[Double] = {
    val norm = ws.sum
    ws map {w => w/norm}
  }

  
  def cummulative(ps: IndexedSeq[Double]): IndexedSeq[Double] = {
    def accsum(cs: IndexedSeq[Double], xs: Seq[Double]): IndexedSeq[Double] =
      xs match {
        case Seq() => cs
        case Seq(y, ys @ _*) => accsum(cs :+ (cs.last + y), ys)
      }
    accsum(IndexedSeq(0.0), normalized(ps))
  }

  def binarySearch(xs: IndexedSeq[Double], x: Double): Option[Int] = {

    def recurse(low: Int, high: Int): Option[Int] = (low + high)/2 match{
      case _ if high < low => None
      case _ if (x > xs(high)) => None
      case mid if (x < xs(mid)) => recurse(low, mid - 1)
      case mid if ((xs(mid) <= x) && ( x < xs(mid+1))) => Some(mid)
      case mid if( xs(mid+1) <= x) => recurse(mid + 1, high)
    }

    recurse(0, xs.length - 1)
  }

  def chooseByBisecting(ps: IndexedSeq[Double])(implicit rg: util.Random): Option[Int] = 
    binarySearch(cummulative(ps), rg.nextDouble)

}
