//a general data structure, array of fixed length
package Collections
import scala.language.postfixOps
import scala.language.implicitConversions


class LoopySeq[T](val length: Int, val start: Int, val elems: IndexedSeq[T])
{
  def apply(i: Int): T = elems( (start + i) % length)

  override def toString: String = "LoopySeq containing " + elems

  def :+(t: T): LoopySeq[T] = {
    if ( elems.length < length) new LoopySeq( length, 0, elems :+ t)
    else new LoopySeq( length, (start + 1) % length, elems.updated(start, t) )
  }

  def :++(xs: Seq[T]): LoopySeq[T] = xs match{
    case Seq() => this
    case Seq(x, ys @ _ *) => (this :+ x) :++ ys
  }
}

object LoopySeq{
  def apply[T](elems: IndexedSeq[T]): LoopySeq[T] =
    new LoopySeq(elems.length, 0, elems)
}

