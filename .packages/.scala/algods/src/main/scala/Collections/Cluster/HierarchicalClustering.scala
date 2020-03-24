/*
 * here lets try something that we can compile
 * 2013 February 04 Bussigny Monday Night
 */

package Collections.Clustering

import Collections.Matrix._
import Collections.Tree._
import collection.immutable._

//we need a measure of similarity measure

object Similarity
{
	type Index = Double
	def SELF = 0.0
	def MIN = 0.0
}
	

//we need an similarity object to order,

class PairSimil( val pair: (Int, Int),
                 val similarity: Similarity.Index) extends Ordered[PairSimil]
{
	def compare(that: PairSimil) = {
		if (this.similarity > that.similarity) 1
		else if (this.similarity < that.similarity) -1
		else{
			if ( that.pair._1 != that.pair._1) this.pair._1 - that.pair._1
			else this.pair._2 - that.pair._2
		}
	}
	override def toString = "similarity of " + pair.toString + ": " + similarity
}

object PairSimil
{
	def apply(c1: Int, c2: Int,
            s: Similarity.Index): PairSimil = {
		if ( c1 < c2) new PairSimil( (c1, c2), s)
		else  new PairSimil( (c2, c1), s)
	}
	def apply(p: (Int, Int),
            s: Similarity.Index): PairSimil = apply( p._1, p._2, s)
}
	

object ExampleTree
{
	def apply(elems: List[PairSimil]) = SortedSet(elems: _*)
}

//we need indexed clusters

trait ClusterIdxd[T] extends Cluster[T] with Indexed

case class ForkIdxd[T](index: Int,
                       left: ClusterIdxd[T],
                       right: ClusterIdxd[T],
                       up: () => Tree[T])  extends ClusterIdxd[T]
{
	def value: Set[T] = left.value ++ right.value

	override def toString =
    "Fork(" + index + "," + value.toString + ": " + left.toString + ", " + right.toString + ")"
}


case class LeafIdxd[T](index: Int,
                       val value: Set[T],
                       up: () => Tree[T]) extends ClusterIdxd[T]
{
	val left = End
	val right = End

	override def toString = "Leaf(" + index + "," + value.toString + ")"
}

object mergeTreesIdxd
{
	def apply[T](x: ClusterIdxd[T],
               y: ClusterIdxd[T],
               m: Int): ForkIdxd[T] = {
		lazy val merged: ForkIdxd[T] = ForkIdxd(m, l, r, () => End)

		lazy val l = x match {
			case ForkIdxd(i, v, w, u) => ForkIdxd(i, v, w, () => merged)
			case LeafIdxd(i, elems, u)  => LeafIdxd(i, elems, () => merged)
		}

		lazy val r = y match {
			case ForkIdxd(i, v, w, u) => ForkIdxd(i, v, w, () => merged)
			case LeafIdxd(i, elems, u)  => LeafIdxd(i, elems, () => merged)
		}
		merged
	}
}
	

//lets use the indexed clusters in class ClusteringLevel

class ClusteringLevel[T](val level: Int,
                         val clusters: Vector[ClusterIdxd[T]],
                         val similMat: Matrix[Similarity.Index],
                         val similSorted: SortedSet[PairSimil])
{
	val numClusters = clusters.size
	
	def similarity(i: Int, j: Int): Similarity.Index = similMat(i, j)

	def similarity(i: Int, j: Int, k: Int): Similarity.Index =
    similarity(i, j) max similarity(i, k)

	def similarity(i: Int, p: (Int, Int)): Similarity.Index =
    similarity(i, p._1, p._2)

	def makePairSimils(i: Int):Vector[PairSimil] = 
	  RangeVec(numClusters) map ( (j: Int) => PairSimil(i, j, similarity(i,j)))

	def makePairSimils(c: ForkIdxd[T]): Vector[PairSimil] = 
		RangeVec(numClusters) map (
      (i: Int) => PairSimil(i, c.index,
                            similarity(i, (c.left.index, c.right.index)))
    )

	def makeVecSimils(i: Int): Vector[Similarity.Index] = 
		RangeVec(numClusters) map ( (j: Int) =>  similarity(i,j)) 

	def makeVecSimils(c: ForkIdxd[T]): Vector[Similarity.Index] =
		RangeVec(numClusters) map (
      (i: Int) => similarity(i, (c.left.index, c.right.index))
    )

	private def +( cnew: ForkIdxd[T]) = {
		//add only at the end of clusters
		require(cnew.index == numClusters)
		val similsOfNew = makeVecSimils(cnew)
		val similMatNew = similMat.
      rowadded(similsOfNew).
      coladded(similsOfNew :+ Similarity.SELF)

		ClusteringLevel(level,
                    clusters :+ cnew,
                    similMatNew,
                    similSorted ++ makePairSimils(cnew))
	}

	private def - (i: Int) = {
		require ( i < numClusters)	
		ClusteringLevel[T](level,
                       clusters,
                       similMat,
                       similSorted -- makePairSimils(i))
	}
	
	def incrementLevel = ClusteringLevel[T](level + 1,
                                          clusters,
                                          similMat,
                                          similSorted)

	def mergeTopTwo() = {
		val topair = similSorted.max.pair
		val cnew = mergeTreesIdxd(clusters(topair._1),
                              clusters(topair._2),
                              numClusters)

		(((this - topair._1) - topair._2) + cnew).incrementLevel
	}
}

object ClusteringLevel
{
	def apply[T](l: Int,
               c: Vector[ClusterIdxd[T]],
               sm: Matrix[Similarity.Index],
               ss: SortedSet[PairSimil]) =
		new ClusteringLevel[T](l, c, sm, ss)
}
	
	
/*
//lets try a simple data structure to represent a clustering

class ClusteringLevel(val level: Int, val mergedInto: Vector[Int], val composedFrom: Map[Int, (Int, Int)], val similMat: Matrix[Similarity.Index], val similSorted: SortedSet(PairSimil)){
	val numClusters = mergedInto.size
	//similarity of i to the merged j,k, this for single linkage clustering level
	def similarity(i: Int, j: Int, k: Int) = {
		require( i < numClusters)
		similMat(i,j) max similMat(i,k)
	}
	def similarity(i: Int, p: (Int, Int)) = {
		require( i < numClusters)
		similMat(i, p._1) max similMat(i, p._2)
	}

	def mergeTopTwo( ): ClusteringLevel = {
		val topair = similSorted.max.pair
		//compute similarities for merged
		val groups = Vector((0 until numClusters): _*)
		val similsToAdd = ( groups map ((i: Int) => similarity(i, topair))) :+ Similarity.MIN
		//this might be too slow
	//	similSorted.filter( (ps: PairSimil) => (ps.pair._1 == topair._1) || (ps.pair._1 == topair._2) || (ps.pair._2 == topair._1) || (ps.pair._2 == topair._2))
		val simPairsToRem1 = groups map ( (i: Int) => PairSimil(topair._1, i, similMat(topair._1, i)))
		val simPairsToRem2 = groups map ( (i: Int) => PairSimil(topair._2, i, similMat(topair._2, i)))
		val simPairsToAdd = groups map ( (i: Int) => PairSimil(i, numClusters, similsToAdd(i)))
		
		val similSortedNew = ((similSorted -- simPairsToRem1) -- simPairsToRem2) ++ simPairsToAdd
		
		val similMatNew = (similMatNew.rowadded( similsToAdd).coladded(similsAdded))
		
		val mergedIntoNew = mergedInto.updated(topair._1, numClusters).updated(topair._2, numClusters)
		val composedFromNew = composedFrom.updated( numClusters, topair)

		ClusteringLevel(l + 1, mergedIntoNew, composedFromNew, similMatNew, similSortedNew)
	}
}

object ClusteringLevel{
	def apply(l: Int, to: Vector[Int], from: Map[Int, (Int, Int)], mat: Matrix[Similarity.Index], srp: SortedSet( PairSimil)) = {
		new ClusteringLevel(l, to, from, mat, srp)
	}
}

object ClusterSomeLinks{
	//comparisons are built hierarchically upon comparisons between leafs. 
	//this could be required from a concrete implementation of an abstract 
	//class or trait. however we want to play around fast, so we can 
	// just provide similarities between leafs as a matrix
	

	//def apply[T] (links: Vector[(T, T)], linkSimils:  Matrix[Similarity.Index]): ClusteringLevel[T] = {
	def apply( leafSimils: Matrix[Similarity.Index]): ClusteringLevel = {
		val N = leafSimils.rows
		//leaves should now be just the integers 0 through N
		//val leaves: Vector[Leaf] = Vector( ( links map ( (l: (T, T)) => Leaf( Set(l._1, l._2), () => End))): _*)
		val leafInto: Vector((0 until N): _*) //i -> i
		//similarities between leaves
		val leafFrom = Map()
		val pairsimilMat = for(i <- (0 until N); j <- (i + 1 until N)) yield PairSimil( i, j, Matrix(i, j))
		val leaveSimilsSorted = SortedSet( pairsimils: _*)
		mergeTillOne(ClusteringLevel(0, leafInto, leafFrom, pairsimilMat, leaveSimilsSorted))
		
		
	def mergeTillOne(clulev: ClusteringLevel): ClusteringLevel = {
		if (clulev.numClusters == 1) this
		else mergeTillOne( mergeTopTwo(clulev))
	}
*/
