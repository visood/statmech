/*
 * we try to implement the clusters, and the associated tree.
 * We use the Huffman coding example as a reference
 * 2013 January 21, (still snowing, nervewrecking slipperiness).
 * 
 * one of the underlying ideas is to implement a graph through its
 * list of links.
 */

// Not certain about how to order Scala syntax, still can write some idear

//a node is basically an int

package Collections.Tree


object RangeVec
{
	def apply( l: Int, h: Int): Vector[Int] = Vector( (l until h) :_*)
	def apply( h: Int): Vector[Int] = apply(0, h)
}


trait Indexed {
	def index: Int
}
	
trait Containing[T] extends Ordered[T]{
	def compare(that: T): Int
	def contents: Set[Any]
	def max(that: T): T
	def min(that: T): T
}


trait Tree[+T]

case object End extends Tree[Nothing]
{
	override def toString = "END"
}

trait Cluster[T] extends Tree[T] with Ordered[Cluster[T]]
{
	def left: Tree[T]
	def right: Tree[T]
	def up: () => Tree[T]
	def value: Set[T]
	def compare(that: Cluster[T]): Int = this.value.size - that.value.size
	def ==(that: Fork[T]) = this.value == that.value
}

case class Fork[T](left: Cluster[T],
                   right: Cluster[T],
                   up: () => Tree[T]) extends Cluster[T]
{
	def value: Set[T] = left.value ++ right.value
	override def toString =
    "Fork(" + value.toString + ": " + left.toString + ", " + right.toString + ")"
}
case class Leaf[T](val value: Set[T],
                   up: () => Tree[T]) extends Cluster[T]
{
	val left = End
	val right = End
	override def toString = "Leaf(" + value.toString + ")"
}

	
object mergeTrees
{
	def apply[T](x: Cluster[T], y: Cluster[T]) = {
		lazy val merged: Cluster[T] = Fork(l, r, () => End)
		lazy val l = x match{
			case Fork(v, w, u) => Fork(v, w, () => merged)
			case Leaf(elems, u)  => Leaf(elems, () => merged)
		}

		lazy val r = y match{
			case Fork(v, w, u) => Fork(v, w, () => merged)
			case Leaf(elems, u)  => Leaf(elems, () => merged)
		}

		merged
	}
}
		

/*
//the above works, so we use this as a scratch to test a few things

class EgIDpair(val int: Int, val dou: Double){
	override def toString = " int, double pair: " + int + ", " + dou
}

object EgIDpair{
	implicit val ord = new Ordering[EgIDpair]{
		def compare (p1: EgIDpair, p2: EgIDpair) = (p1.int - p2.int).toInt

	}
	def apply(i: Int, d: Double) = new EgIDpair(i, d)
}

object IdPair{
	implicit val ord = new Ordering[(Int, Double)] {
		def compare( p1: (Int, Double), p2: (Int, Double)) = p1._1 - p2._1
	}
	def apply(i: Int, d: Double) = (i, d)
}
*/
