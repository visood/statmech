package Algorithms

import java.io._
import scala.io.Source
import scala.collection.SortedSet
import scala.util.Sorting
import scala.annotation.tailrec



object GreedyMinWeight {
  
  def readData(fn: String): Array[(Int, Int)] = {  //this should be able to deal with any space separated matrix data
    val lines = Source.fromFile(fn).getLines.toArray.map{ line => line.split(' ').map(_.toInt)}
    lines.map{ case Array(x,y) => (x, y)}
  }
  type Job = (Int, Int)
  object JobOrdDiff extends Ordering[Job] {
    def compare(a: Job, b: Job): Int = {
      val c = (a._1 - a._2) compare (b._1 - b._2)
      if (c == 0) -1*(a._1 compare b._1) else -1*c
   }
  }

  object JobOrdRat extends Ordering[Job]{
    def compare(a: Job, b: Job): Int = {
      val ha = (a._1.toDouble / a._2.toDouble)
      val hb = (b._1.toDouble / b._2.toDouble)
      if (ha == hb) -1*(a._1 compare b._1) else -1*(ha compare hb)
   }
  }

  def schedule(data: Array[Job], JobOrdering: Ordering[Job]): (Array[Job], Double) = {
    Sorting.quickSort(data)(JobOrdering)
    (data, data.foldLeft((0.0, 0.0)){ case ( (h, t), (w, l) ) => (h + w*(t+l), t+l) }._1)
  }

  def apply(fn: String): (Double, Double) = {
    val data = readData(fn)
    (schedule(data, JobOrdDiff)._2, schedule(data, JobOrdRat)._2)
  }
}



object Prims{
  type Graph = Map[Int, List[(Int, Int)]]
  type Link = (Int, Int, Int) //head, tail, weight
  
  def addLink(link: Array[Int], network: Graph ): Graph = 
    network.updated( link(0), (link(1), link(2))::network.getOrElse(link(0), List())).updated( link(1), (link(0), link(2))::network.getOrElse(link(1), List()))
  
  def readNetwork(fn: String): Graph = {
    val lines = Source.fromFile(fn).getLines.toArray.map{ line => line.split(' ').map(_.toInt)}
    lines.foldLeft (Map[Int, List[(Int, Int)]]()) {case (net, link) => addLink(link, net)}
  }

  object LinkOrd extends Ordering[ Link] {
    def compare(a: Link, b: Link): Int = -1*(a._3 compare b._3)
  }

  def linksOf(node: Int, net: Graph): List[(Int, Int, Int)] = 
   net.getOrElse(node, List()).map( (nws: (Int, Int)) => (node, nws._1, nws._2))


  def MST(net: Graph): List[Link] = {
    val frontier = scala.collection.mutable.PriorityQueue[Link]( linksOf(net.keys.head, net):_*)(LinkOrd)
    val explored = scala.collection.mutable.Set(net.keys.head)
    val tree = scala.collection.mutable.Set[Link]()
    //var itr = 0    
    while (frontier.length > 0){
      //println(itr + "\t" + frontier.length)
      //itr += 1
      val step = frontier.dequeue
      if (! explored.contains(step._2)){
        tree += step
        explored += step._2
        //frontier ++= linksOf( step._2, net) filter ( (l: Link) => ! explored.contains(l._2))
        frontier ++= linksOf( step._2, net)
      }
     }
     tree.toList
   }
}




object HammingClusters{
  //case class Cluster( seqLength: Int,  core: Set[Int], halo1: Set[Int], halo2: Set[Int]){
  case class Cluster( seqLength: Int,  core: scala.collection.mutable.Set[Int]){
    lazy val halo1: scala.collection.mutable.Set[Int] = core.flatMap( (s: Int) => singleFlips(s, seqLength))
    //lazy val halo1: scala.collection.mutable.Set[Int] = (scala.collection.mutable.Set[Int]() /: core.map( (s: Int) => singleFlips(s, seqLength)))(_.union(_))
    lazy val halo2: scala.collection.mutable.Set[Int] = core.flatMap( (s: Int) => doubleFlips(s, seqLength)) //worry about the same element in core, and its halos
    //lazy val halo2: scala.collection.mutable.Set[Int] = (scala.collection.mutable.Set[Int]() /: core.map( (s: Int) => doubleFlips(s, seqLength)))(_.union(_))
    def size: Int = core.size

    def mergedWith(that: Cluster): Cluster = {
     if (this.size >= that.size) {
       println("merging cluster of size " + that.size + " into " + this.size)
       core ++= that.core
       halo1 ++= that.halo1
       halo2 ++= that.halo2
       //Cluster(seqLength, this.core union that.core)
       //Cluster( seqLength,  this.core union that.core, this.halo1 ++ that.halo1, this.halo2 ++ that.halo2)
       //Cluster( seqLength,  this.core union that.core, (this.halo1 -- that.core -- that.halo2) ++ (that.halo1 -- this.core -- that.halo2), (this.halo2 -- that.core -- that.halo1) ++ (that.halo2 -- this.core -- this.halo1))
       this
     }
     else that mergedWith this
    }
    def +(that: Cluster): Cluster = mergedWith(that)
    
    def separation(that: Cluster): Int = {
      if (this.size >= that.size) that.core match{
        case cr if cr.exists(core.contains(_)) => 0
        case cr if cr.exists(halo1.contains(_)) => 1
        case cr if cr.exists(halo2.contains(_)) => 2
        case _ => 3
      }
      else that separation this
    }
 
    def grownIn(cs: List[Cluster], maxStep: Int): (Cluster, List[Cluster]) = {
     if (this.size > 1)
       println(this.size + " grow in " + cs.length)
     @tailrec def absorbedIntoHead(c: Cluster, rem: List[Cluster], excl: List[Cluster]): (Cluster, List[Cluster], List[Cluster]) = {
       //println(c.toString + "absorb into head of remaining" + rem.toList + " with excluded " + excl.toList)
       if (rem.isEmpty) (c, Nil, excl)
       else {
         if (c.separation(rem.head) > maxStep) absorbedIntoHead(c, rem.tail, rem.head::excl)
         else {
            val (c1,  excl1) =  rem.head grownIn(excl, maxStep)
            //val (c1, rem1, excl1) =  absorbedIntoHead(rem.head, excl, Nil)
            if (c1.size > 1)
            println("merge clusters of sizes " + c.size + ", " + c1.size + " remaining " + rem.length + " excluded " + excl1.length)
            absorbedIntoHead(c + c1, rem.tail, excl1)
          }
        }
     }
     val (c, r, e) = absorbedIntoHead(this, cs, Nil)
     if (this.size > 1)
       println("grown  to size " + c.size + " in  " + cs.size + " leaving " + e.size + " check sizes " + cs(0).size + " this " + this.size)
     (c, e)
   }
  }

  
  def intVal(bs: List[Short]): Int = ( 0 /: bs) { case (s, b) => 2*s + b}

  def bits(i: Int): List[Short] = {
    def bitsReversed(j: Int): List[Short] = {
      if (j == 0) Nil
      else (j % 2).toShort::bitsReversed(j >>> 1)
    }
    bitsReversed(i).reverse
  }

  def numUpBits(i: Int): Int = if (i == 0) 0 else i%2 + numUpBits(i >> 1)

  def readData(fn: String): List[Int]  = 
    Source.fromFile(fn).getLines.toList.map{ line => intVal(line.split(' ').map(_.toShort).toList)}

  def singleFlips(s: Int, n: Int): Set[Int] = Set((for(i <- 0 until n) yield (s ^ (1 << i))):_*)
  def doubleFlips(s: Int, n: Int): Set[Int] = {
    Set((for(i <- 0 until n; j <- i + 1 until n) yield ( s ^ ( ( 1 << i ) | (1 << j)))):_*)
  }

  def makeClusters( spins: List[Int], seqLength: Int): List[Cluster] = 
    spins.map( (s: Int) => Cluster(seqLength , scala.collection.mutable.Set(s)))


 
  def maximal(c: Cluster, cs: List[Cluster]): (Cluster, List[Cluster]) = {
    def grow(c1: Cluster, rem: List[Cluster], excl: List[Cluster]): (Cluster, List[Cluster], List[Cluster]) = {
      if (rem.isEmpty) (c1, rem, excl)
      else{
        if ( c1.halo1.exists(rem.head.core.contains(_))){
          val (c2, rem2, excl2) = grow( rem.head, excl, Nil)
          grow(c1 mergedWith c2, rem.tail, excl2)
        }
        else grow(c1, rem.tail, rem.head::excl)
      }
   }
   val (cf, r, e) = grow(c, cs, Nil)
   (cf, e)

 }

  def components(es: List[Cluster], d: Int): List[Cluster] = {
    def comps1(acc: List[Cluster], cs: List[Cluster]): (List[Cluster], List[Cluster]) = {
      println( "found " + acc.length +  " search for a connected component in " + cs.length + " nodes ")
       cs match {
         case Nil => (acc, Nil)
         case x::xs => {
           val (c, es) = x grownIn(xs, d)
           comps1(c::acc, es)
         }
       }
     }
    comps1(List[Cluster](), es)._1
   }
   
}


case class UnionFind[T]( leader: Map[T, T], followers: Map[T, Int], numLeaders: Int){

  def find(x: T): T = if (leader(x) != x) find(leader(x)) else x

  lazy val clusterLabels: List[T] = leader.keys.map(find(_)).toList

  def nodes: List[T] = leader.keys.toList

  lazy val clusters: Map[T, List[T]] = {
    val cls: Map[T, List[T]] = clusterLabels.zip(Seq.fill(numLeaders)(List[T]())).toMap
    (cls /: nodes){case (m, n) => m.updated(find(n), n::m(find(n)))}
  }
 
  def trail(x: T): Stream[T] = if (leader(x) != x) x#::trail(leader(x)) else x#::Stream[T]()

  def trailReversed(x: T): List[T] = trail(x).reverse.toList
    
  def union(x: T, y: T): UnionFind[T] = {
    if (followers(find(x)) <= followers(find(y))){
      val trly = trail(y).reverse.toList
      val trlx = trail(x).reverse.toList
      //val ly = find(y); val lx = find(x);
      val newLeaders =  ( leader /: (trlx ++ trly)){case(m, u) => m.updated(u, trly.head)}
      if (trly.head != trlx.head){
        val newFoll = followers.updated(trlx.head, 0).updated(trly.head, followers.getOrElse(trly.head, 0) + followers(trlx.head))
        UnionFind(newLeaders, newFoll, numLeaders - 1)
      }
      else UnionFind(newLeaders, followers, numLeaders)
    }
    else union(y, x)
  }
}


object ClusteringExample{
  def readDisMatrix(fn: String): Map[Int, Map[Int, Int]] = {
    val data = Source.fromFile(fn).getLines.toList.map{ line => line.split(' ').map(_.toInt).toList}
    val defDis = data.map( l => l(2)).max + 1
    ( Map[Int, Map[Int, Int]]().withDefault( _ => Map[Int, Int]().withDefault( _ => defDis)) /: data)
      { case (m, l) => m.updated( l(0), m(l(0)).updated(l(1), l(2))).updated( l(1), m(l(1)).updated(l(0), l(2))) }
  }
  
  def readDisPairs(fn: String): List[ ((Int, Int), Int)] = {
    val data = Source.fromFile(fn).getLines.toList.map{ line => line.split(' ').map(_.toInt).toList}
    data.map{ l => ( (l(0), l(1)), l(2))}
  }

  def distancePairs(dismat: Map[Int, Map[Int, Int]]): List[ ((Int, Int), Int)] = 
    (for( (i, m) <- dismat; (j, d) <- m) yield ( (i, j), d)).toList

  def distanceMatrix(disp: List[ ( (Int, Int), Int)]): Map[ Int, Map[Int, Int]] = {
    val defDis = disp.map(_._2).max + 1
    ( Map[Int, Map[Int, Int]]().withDefault(_ => Map[Int, Int]().withDefault( _ => defDis)) /: disp)
      { case (m, ((i, j), d)) => m ++ List( i -> m(i).updated(j, d), j -> m(j).updated(i, d))}
  }
 
  object LinkOrd extends Ordering[ ( (Int, Int), Int)] {
    def compare(a: ( (Int, Int), Int), b: ( (Int, Int), Int)): Int = -1*(a._2 compare b._2)
  }
  
  def allNodes(disps: List[ ( (Int, Int), Int)]): List[Int] = 
    disps.flatMap{ case ( (x, y), d) => List(x, y)}.removeDuplicates

  def separation(c1: List[Int], c2: List[Int], dist: Map[Int, Map[Int, Int]]): Int = 
    (for(n <- c1; m <- c2; if (n != m) ) yield dist(n)(m)).min

  def clusterSeparations(uf: UnionFind[Int], dist: Map[Int, Map[Int, Int]]): List[ ((Int, Int), Int)] = 
    for(ci <- uf.clusterLabels; cj <- uf.clusterLabels; if ( ci != cj)) yield ( (ci, cj), separation(uf.clusters(ci), uf.clusters(cj), dist))
  
    
    

  def clustering( disps: List[ ( (Int, Int), Int) ], k: Int): (UnionFind[Int], scala.collection.mutable.PriorityQueue[ ( (Int, Int), Int) ]) = {
    val dispq = scala.collection.mutable.PriorityQueue[ ( (Int, Int), Int) ]( disps:_* )(LinkOrd)
    val nodes = allNodes(disps)
    var uf = UnionFind[Int]( Map( nodes.zip(nodes):_*), Map[Int, Int]().withDefault((i: Int) => 0), nodes.length)
    var numClusters = nodes.length
    while(uf.numLeaders > k) {
      val ( (x, y), d) = dispq.dequeue
      if( uf.find(x) != uf.find(y)){
        println(" before update, UF: " + x + " --> " + uf.find(x) + ", " + y + " --> " + uf.find(y))
        uf = uf.union(x, y)
        println(uf.numLeaders + " clusters, next link considered " + x + "--" + y + ": " + d)
        println(" updated UF: " + x + " --> " +  uf.find(x) + ", " + y + " --> " + uf.find(y))
        println("===========================================================================")
     }
    } 
    (uf, dispq)
  }

  def maxSpacingOfClustering( disps: List[ ( (Int, Int), Int)], k: Int): Int = {
    val (uf, pq): (UnionFind[Int], scala.collection.mutable.PriorityQueue[ ( (Int, Int), Int)]) = clustering(disps, k)
    clusterSeparations(uf, distanceMatrix(disps)).map(_._2).min
  }

}



case class KnapSack(weights: IndexedSeq[Int], values: IndexedSeq[Int]){
  def solve(C: Int): Map[ (Int, Int), Int]   = {
    def solveWithTable(  wtCap: Int, itm: Int, tab: Map[ (Int, Int), Int ] ):   Map[ (Int, Int), Int ] = {
      //println("solve upto index " + itm + " capacity "  + wtCap)
      (wtCap, itm) match {
        case (_, -1) => tab + ( (wtCap, itm) -> 0 )
        case (c, i) if (tab.isDefinedAt( (c, i))) => tab
        case (c, i) if ( c < weights(i) ) => {
          val tab1 = solveWithTable(c, i - 1, tab)
          tab1 + ( (c, i) -> tab1(c, i - 1))
        }
          
        case _ => {
          val excluded =  solveWithTable(wtCap, itm - 1, tab)
          val w = weights(itm)
          val included =   solveWithTable(wtCap - w, itm - 1, excluded)
          if ( included( (wtCap - w, itm - 1) ) + values(itm) > excluded(wtCap, itm - 1) ) {
            included + ( (wtCap, itm) -> (included(wtCap - w, itm - 1) + values(itm)))
          }
          else {
            included + ( (wtCap, itm) -> excluded(wtCap, itm - 1))
          }
        }
      }
    }
    val n = weights.length - 1
    solveWithTable( C, n, Map[ (Int, Int), Int]())
  }
  
  def reconstruct(tab: Map[ (Int, Int), Int], wcap: Int, n: Int): List[Int] = (wcap, n) match{
    case (w, i) if ( i < 0) => List[Int]()
    case (w, i)  if (tab(w, i) == tab(w, i - 1)) => reconstruct(tab, w, i - 1)
    case (w, i) if (tab(w, i) == tab(w - weights(i), i - 1) + values(i)) => i::reconstruct(tab, w - weights(i), i - 1)
    case _ => throw new IllegalArgumentException(" not a good table ")
  }
}

object KnapSackMemoized{ //maintains state
  val table = scala.collection.mutable.Map[(Int, Int), Int]()
  def memoize(c: Int, i: Int, V: Int): Int = {
    table += ((c, i) -> V)
    V
   }
  def apply( wts: IndexedSeq[Int], vls: IndexedSeq[Int], c: Int): Int = {
    //println("solve for capacity " + c + "\t remaining items " + wts.length)
   (c, wts.length) match{
    case (_, 0) => 0
    case (c, i)  if table.isDefinedAt(c, i) => table(c, i)
    case (c, i) if (c < wts.last) => memoize( c, i,  apply(wts.init, vls.init, c))
    case (c, i) => memoize( c, i, apply(wts.init, vls.init, c) max (apply(wts.init, vls.init, c - wts.last) + vls.last) )
   }
 }
}
  


object KnapSackData{
  def read(fn: String): List[(Int, Int) ] = {
    val data = Source.fromFile(fn).getLines.toList.tail.map{ line => line.split(' ').map(_.toInt)}
    data.map{ case l => (l(0), l(1) ) }
  }
}


//shortest paths
/*
object GraphSearch{ //a stub for now
	def searchBF(explored: Set[V], s: V) : Set[V] = {
		def explore(expld: Set[V], frontier: IndexedSeq[V]): (Set[V], Set[V]) = frontier match{
			case IndexedSeq.empty => (expld, Nil)
			case Seq(x, fs@_*)  => if (expld(x)) explore(expld, fs) else explore(expld + x, frontier :+ neighbors(x) )
		}
	}
	//def connectedTo( edges: List[ (T, T) ], s: T): Set[T]
	//def connectivity[V]( edges: List[ (T, T) ] ): Boolean
	
	//def dijkstraShortestPath( adjList: Map[V, List[V]])(expld: Set[V], s: V, v: V): 

}
*/

object GraphUtils{
  def randomDirected(n: Int, p: Double, wmin: Int, wmax: Int)(implicit rg: util.Random): List[ (Int, Int, Int) ] = 
    ( for( i <- 0 until n; k <- 0 to (rg.nextGaussian + p*n).toInt) yield (i, rg.nextInt(n), wmin + rg.nextInt(wmax - wmin) ) ).toList

}

class ShortestPath[V](val links: List[ (V, V, Int) ])(implicit val Vord: Ordering[V] ){
  
  case class Neighbor( node: V, distance: Int)
  case class Edge( head: V, tail: V, dist: Int)


  val Inf : Int = (0 /: links.iterator){ case ( s, (_, _, l )) => s + math.abs(l)} + 1 //practically infinity, no path can be as long as all the edges plus 1 !!!

  lazy val nodes: List[V] = ( SortedSet.empty(Vord) /: links){case (s, (x, y, _)) => (s + x) + y}.toList

  lazy val adjlist: Map[V, List[Neighbor] ] = {
    def add(al: Map[V, List[ Neighbor ] ], l: ( V, V, Int )): Map[V, List [ Neighbor] ] = 
      al + ( l._1 -> (Neighbor( l._2, l._3)::al(l._1)) )
    ( Map[V, List[Neighbor] ]().withDefault( (v: V) => Nil) /: links)(add(_, _))
  }

  lazy val inlist: Map[V, List[Neighbor] ] = {
    def add(il: Map[V, List[ Neighbor ] ], l: ( V, V, Int )): Map[V, List [ Neighbor] ] = 
      il + ( l._2 -> (Neighbor( l._1, l._3)::il(l._2)) )
    ( Map[V, List[Neighbor] ]().withDefault( (v: V) => Nil) /: links)(add(_, _))
  }

  def outEdges( v: V): List[Edge] =  adjlist(v).map{ case Neighbor(u, d) => Edge(v, u, d)}

  def inEdges(v: V): List[Edge] =  inlist(v).map{ case Neighbor(u, d) => Edge(u, v, d)}

  lazy val edgeLengths: Map[ (V, V), Int] = {
    def add(el: Map[ (V, V), Int], l: (V, V, Int) ): Map[ (V, V), Int] = el + ( (l._1, l._2) -> l._3)
    ( Map.empty.withDefault( (ij: (V, V)) => Inf) /: links)( add( _, _) )
  }


  object NbrOrd extends Ordering[Neighbor]{
    def compare( a: Neighbor, b: Neighbor) = (a, b) match{
      case (Neighbor(m, x), Neighbor(n, y)) if ( (x == y) && (m != n) ) => -1
      case (Neighbor(_, x), Neighbor(_, y) ) => x compare y
    }
  }

  object EdgeOrdMax extends Ordering[Edge]{
    def compare(a: Edge, b: Edge) = (a, b) match{
      case (Edge(x, y, c), Edge(u, v, d)) if ( (x == u) && (y == v) ) => c compare d
      case (Edge(_, _, c), Edge(_, _, d)) => 2*( c compare d) compare 1
    }
  }

  object EdgeOrdMin extends Ordering[Edge]{
    def compare(a: Edge, b: Edge) =  -1*EdgeOrdMax.compare(a, b)
  }

  def DijkstraSPss(s: V): Map[V, Int] = { //Dijkstra with sorted set
    def explore( expldis: Map[V, Int], frtr: SortedSet[ Edge ] ): Map[V, Int] = {
      println(" frontier of size " + frtr.size)
      if (frtr.isEmpty) expldis
      else frtr.head match{
        case Edge(x, y, d) if (! expldis.contains(y)) => explore( expldis + (y -> (expldis(x) + d)), frtr.tail ++ outEdges(y))
        case Edge(x, y, d) if ( expldis(x) + d < expldis(y) ) => explore( expldis + (y -> (expldis(x) + d)), frtr.tail)
        case _ => explore( expldis, frtr.tail) 
      }
    }
    explore( Map[V, Int](s->0), SortedSet.empty( EdgeOrdMax) ++ outEdges(s) ) //SortedSet orders min to max , with min at head
  }

  def DijkstraSP(s: V): Map[V, Int] = { //Dijkstra with priority queue
    println(" Dijkstra from vertex " + s)
    val frtr = scala.collection.mutable.PriorityQueue[Edge]( outEdges(s):_* )( EdgeOrdMin)
    val expldis = scala.collection.mutable.Map[V, Int](s->0)
    while (frtr.length > 0){
      //println("frontier of length" + frtr.length)
      frtr.dequeue match {
        case Edge(x, y, d) if ( ! expldis.contains(y) ) => { expldis(y) =  expldis(x) + d; frtr ++= outEdges(y)}
        case Edge(x, y, d) =>  if ( expldis(x) + d < expldis(y) ) { expldis(y) = expldis(x) + d}
      }
    }
    expldis.toMap
  }

  def allNodeDijkstraSP: Map[(V, V), Int] = Map((for(s <- nodes; (v, o) <- DijkstraSP(s))  yield ( (s, v)->o)):_*)

  object InfOrd extends Ordering[ Option[Int] ]{
    def compare(a: Option[Int], b: Option[Int]) = (a, b) match {
      case (None, None) => 0
      case (None, _) => 1
      case (_, None) => -1
      case (Some(i), Some(j)) => i compare j
    }
  }

  def BellmanFord(s: V): Option[Map[V, Option[Int]] ]  = {
    def viaNeighs(crt: Map[V, Option[Int]], v: V): Option[Int] = {
      val nops: List[Option[Int]] = inlist(v).map{ case Neighbor(u, l) => crt(u).map{ case d => d + l}} 
      (None.asInstanceOf[Option[Int]] /: nops){ case (a, b) => InfOrd.min(a, b)}
      //(nops.head /: nops.tail){ case (a, b) => InfOrd.min(a, b)}
    }
  
    def nxtDis(crtDis: Map[V, Option[Int]]): Map[V, Option[Int]] = Map(nodes.map{ case v => (v, InfOrd.min(crtDis(v), viaNeighs(crtDis, v)))}:_*)

    def distances(l: Int, crt: Map[V, Option[Int]]): Option[ Map[V, Option[Int]]] = (l, nxtDis(crt)) match{
      case (_, nxt) if (nxt == crt) => Some(nxt)
      case (m, nxt) if (m == nodes.length - 1) => if (nxt == crt) Some( crt ) else None
      case (m, nxt) => distances(m + 1, nxt)
    }
    distances( 0, Map[V, Option[Int]](s -> Some(0)).withDefault( (v: V) => None))
  }

  //def FloydWarshall:  Map[ V, Map[V, Option[Int]]] = {
  def FloydWarshall:  Map[ (V,V),  Option[Int]] = {
    
    def nxtDis( crtDis: Map[(V, V), Option[Int]], v: V): Map[ (V, V), Option[Int]] = {
      println( "proceeding from vertex " + v.toString)
      Map(( for(u <- nodes; w <- nodes) yield ( (u, w), InfOrd.min( crtDis(u, w), for( uv <- crtDis(u, v); vw <- crtDis(v, w) ) yield (uv + vw) ) ) ):_*)
    }

    val initial: Map[(V, V), Option[Int]] = Map( (( for(u <- nodes; Neighbor(v, l) <- adjlist(u)) yield( (u, v) -> Some(l))) ++ (for(u <- nodes) yield ((u, u) -> Some(0)))):_*).withDefault( (xy: (V, V) ) => None.asInstanceOf[Option[Int]])
    ( initial /: nodes){ case (crtDis, v) => nxtDis(crtDis, v) }
  }

  def FloydWarshallPaths: Map[ (V, V), List[V]] = { 
    def pathLength(p: List[V]): Int = if (p.length < 2) 0 else (p zip p.tail).map(edgeLengths).sum
    def shorter( p1: List[V], p2: List[V]): List[V] = (p1, p2) match {
      case (Nil, Nil) => Nil
      case (Nil, p) => p
      case (p, Nil) => p
      case _ => if (pathLength(p1) < pathLength(p2) ) p1 else p2
    }
    def combine(p1: List[V], p2: List[V]): List[V] = (p1, p2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x::Nil, y::ys) => if (x == y) x::ys else x::y::ys
      case (x::xs, ys) =>  x::combine(xs, ys)
    }


    def nxt( paths: Map[ (V, V), List[V]], v: V): Map[ (V, V), List[V]] = 
      (for( u <-  nodes; w <- nodes) yield ( (u, w) -> shorter( paths(u,w), combine( paths(u,v), paths(v, w)) )) ).toMap
         
    val initial = (links.iterator.map{case (x, y, _) => ( (x, y), List(x,y) )} ++ nodes.iterator.map{ case x => ((x, x), List(x))}).toMap.withDefault( (xy: (V, V)) => Nil)
    ( initial /: nodes)( nxt )
  }

  def betweennessCentrality: Map[V, Int] = {
    FloydWarshallPaths.values.flatten.groupBy(x => x).map{ case (x, l) => (x, l.map( i => 1).sum)}
   //( Map.empty.withDefault( (x: V) => 0) /: (for( path <- FloydWarshallPaths.values, v <- path) yield(v))){ case (m, v) => m.updated( v, m(v) + 1) }
  }



  def Johnson(s: V): Option[Map[ (V, V), Int]] = 
    ShortestPath( nodes.map{ case v => (s, v, 0)} ++ links).BellmanFord(s).map{
      case bfDis =>  ShortestPath( links.map{ case (u, v, l) => (u, v, l + bfDis(u).get - bfDis(v).get)} ).allNodeDijkstraSP.map{ case ( (u, v), l) => (u, v)->(l - bfDis(u).get + bfDis(v).get )}
    }
        
   
}
object ShortestPath{
  def apply[V](links: List[ (V, V, Int) ] )(implicit vord: Ordering[V]): ShortestPath[V] =  new ShortestPath(links)
}

case class PlaneTSP1(val cities: IndexedSeq[(Double, Double)]){
// this works for TSP for points in a 2D plane, 

  def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
    { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}

  def length(p: List[Int]): Double = 
    if (p.length < 2) 0.0
    else (p zip p.tail).map{ case (i, j) => distance(i, j)}.sum

  object LenOrd extends Ordering[ List[ Int ] ]{
    def compare(a: List[ Int ], b: List[ Int] ) = length(a) compare length(b)
  }

  def subsetIndex( path: List[Int]): Int = (0 /: path){ case (i, c) => i + (1 << c) }
    


  def appendCity(path: List[ Int ]): Set[List[ Int]] = 
    (for(c <- (0 until cities.length)  if (! path.contains(c) )) yield c::path).toSet

  def nxt(crt: Set[List[ Int ]]): Set[ List[ Int] ] = 
    crt.flatMap(appendCity).groupBy( (xs: List[Int]) => (subsetIndex(xs), xs.head)).map{ case ( _, xss) => xss.min(LenOrd)}.toSet
    //crt.flatMap(appendCity).groupBy(_.head).map{ case (e, ps) => ps.min(LenOrd)}.toSet

  lazy val solution = ( Set(List(0)) /: (1 until cities.size)){case (crt, _) => nxt(crt)}.map(0::_).min(LenOrd)
}




class PlaneTSP2(val cities: IndexedSeq[(Double, Double)]){
// this works for TSP for points in a 2D plane, 

  def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
    { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}

  def length(p: List[Int]): Double = 
    if (p.length < 2) 0.0
    else (p zip p.tail).map{ case (i, j) => distance(i, j)}.sum

  object LenOrd extends Ordering[ List[ Int ] ]{
    def compare(a: List[ Int ], b: List[ Int] ) = length(a) compare length(b)
  }

  def subsetIndex( path: List[Int]): Int = (0 /: path){ case (i, c) => i + (1 << c) }

  def subsetInclude(si: Int, c: Int): Int = si + (1 << c)

  def subsetContains(si: Int, c: Int): Boolean = (si >> c)%2 == 1
 
  def setOfSubsetSize(N:Int)(m: Int): Set[Int] = 
    ( Set[Int](0) /: (0 until m)){ case (ss, n) => for(s <- ss; c <- (0 until N) if (! subsetContains(s, c))) yield (subsetInclude(s, c))}

  def appendCity( path: ((Int, Int), Double)): Map[ (Int, Int), Double] = {
    val grown = scala.collection.mutable.Map[ (Int, Int), Double]()
    for( c <- ( 0 until cities.length) if (! subsetContains(path._1._1, c)))
    { 
        grown( (subsetInclude(path._1._1, c), c) ) = distance(path._1._2, c) + path._2 
    }
    grown.toMap
    //(Map[ (Int, Int), Double]() /: (0 until cities.length) ){ 
  //case (m, c) => if (! subsetContains(path._1._1, c)) m.updated( (subsetInclude(path._1._1, c), c), distance(path._1._2, c) + path._2) else m
    //}
  }



  def nxt( crt: Map[ (Int, Int),  Double]): Map[ (Int, Int), Double] = {
    println( " next from ", crt.size)
    val branched: Map[ ( (Int, Int), (Int, Int) ), Double]  = for( ( (si, c1), d) <- crt; c2 <- (0 until cities.length) if (!subsetContains( si, c2) )) yield {
      ( ( (subsetInclude(si, c2), c2), ( si, c1) ), d + distance(c1, c2) )
    }
    branched.groupBy( _._1._1).map{ case (k, v) => (k, v.values.min)}
  }

  def solveFrom(s: Int) = ( Map( (1 << s, s) -> 0.0) /: (1 until cities.length)){ case (m, _) => nxt(m) }.map{ case ( (_, c), d) => d + distance(c, s)}.min

  lazy val solve = solveFrom(0)
}

object PlaneTSP{
  def apply(cities: IndexedSeq[(Double, Double)]): PlaneTSP ={
    def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
      { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}
    new PlaneTSP((0 until cities.length).sortBy( c => distance(c, 0) ).map( i => cities(i)))
  }
}




class PlaneTSP(val cities: IndexedSeq[(Double, Double)]){
  import scala.collection.mutable.{ Map => MuMap}
// this works for TSP for points in a 2D plane, 

  val N = cities.length 

  def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
    { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}

  def length(p: List[Int]): Double = 
    if (p.length < 2) 0.0
    else (p zip p.tail).map{ case (i, j) => distance(i, j)}.sum

  object LenOrd extends Ordering[ List[ Int ] ]{
    def compare(a: List[ Int ], b: List[ Int] ) = length(a) compare length(b)
  }

  def appendCity( path: ((Int, Int), Double)): Map[ (Int, Int), Double] = {
    val grown = scala.collection.mutable.Map[ (Int, Int), Double]()
    for( c <- ( 0 until cities.length) if (! subsetContains(path._1._1, c)))
    { 
        grown( (subsetInclude(path._1._1, c), c) ) = distance(path._1._2, c) + path._2 
    }
    grown.toMap
  }
  def setOfSubsetSize(N:Int)(m: Int): Set[Int] = 
    ( Set[Int](0) /: (0 until m)){ case (ss, n) => for(s <- ss; c <- (0 until N) if (! subsetContains(s, c))) yield (subsetInclude(s, c))}

  def subsetIndex( path: List[Int]): Int = (0 /: path){ case (i, c) => i & (1 << c) }
  def subsetInclude(si: Int, c: Int): Int = si & (1 << c)
  def subsetContains(si: Int, c: Int): Boolean = (si >> c)%2 == 1
  def growSetsByOne(N: Int)(ss: Set[Int]): Set[Int] = {
    for( s <- ss; e <- (0 until N).map( 1 << _) if ( (s & e) == 0)) yield ( s ^ e)
    //val nss = scala.collection.mutable.ArrayBuffer[Int]()
    //for( s <- ss; e <- (0 until N).map( 1 << _) if ( (s & e) == 0)) { nss += s ^ e}
    //for( s <- ss; c <- (0 until N) if (! subsetContains(s, c))){ nss += (subsetInclude(s, c))}
    //nss.toSet
  }

  def setOfSubsetSize(N:Int)(m: Int): Set[Int] = {
    if (m == 0) Set(0)
    else growSetsByOne(N)( setOfSubsetSize(N)(m - 1))
  }

  def nxt(n: Int, ss: List[Int], crt: Map[ (Int, Int), Double]) : Map[ (Int, Int), Double] = {
    val branched = MuMap[ ((Int, Int), (Int, Int)), Double]()
    val grown = MuMap[ (Int, Int), Double]()
    for( c <- (0 until N); s <- ss)
    
      grown( (s, c1) ) = (for( r <- subsetRemove(s, c1); c2 <- subsetElements(r)) yield ( crt(r, c2) + distance(c2, c1) )).min
      
  
  def nxt(crt: Map[ (Int, Int), Double]): Map[ (Int, Int), Double] = {
    val branched = scala.collection.mutable.Map[ ( (Int, Int), (Int, Int) ), Double]()
    for( (si, c1, d) <- crt; c2 <- (0 until cities.length) if (!subsetContains(si, c2))){
      branched( ( (subsetInclude(si, c2), c2), ( si, c1) ) ) = d + distance(c1, c2)
    }

  def nxt( crt: Map[ (Int, Int),  Double]): Map[ (Int, Int), Double] = {
    println( " next from ", crt.size)
    val branched: Map[ ( (Int, Int), (Int, Int) ), Double]  = for( ( (si, c1), d) <- crt; c2 <- (0 until cities.length) if (!subsetContains( si, c2) )) yield {
      ( ( (subsetInclude(si, c2), c2), ( si, c1) ), d + distance(c1, c2) )
    }
    branched.groupBy( _._1._1).map{ case (k, v) => (k, v.values.min)}
  }

  def solveFrom(s: Int) = ( Map( (1 << s, s) -> 0.0) /: (1 until cities.length)){ case (m, _) => nxt(m) }.map{ case ( (_, c), d) => d + distance(c, s)}.min

  lazy val solve = solveFrom(0)
}

object PlaneTSP{
  def apply(cities: IndexedSeq[(Double, Double)]): PlaneTSP ={
    def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
      { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}
    new PlaneTSP((0 until cities.length).sortBy( c => distance(c, 0) ).map( i => cities(i)))
  }
}






class PlaneTSP(val cities: IndexedSeq[(Double, Double)]){
  import scala.collection.mutable.{ Map => MuMap}
  val N = cities.length 
  val citySet = (0 until N).toSet
  val distance: IndexedSeq[ IndexedSeq[Double]] = {
    for( i <- 0 until N) yield { 
         for(j <- 0 until N) yield { ( cities(i), cities(j)) match{
           case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
        }
      }
    }
  }

  val infinity = distance.flatten.sum
      
  def subsetContains(s: Int, c: Int): Boolean = (s & (1<<c)) != 0
  
  def subsetAppended(s: Int, c: Int): Int = ( s | ( 1 << c) )
  
  def subsetExpanded(s: Int): Set[Int] =  //filtering may not be necessary
    for(c <- citySet if ( ! subsetContains(s, c))) yield subsetAppended(s, c)

  def subsetRemoved(s: Int, c: Int): Int = ( s ^ ( 1 << c) )

  def subsetElements(s: Int): Set[Int] = for( c <- citySet if ( subsetContains( s, c)) ) yield c

  def nxt( crt: MuMap[ Int, MuMap[ Int,  Double]]) : MuMap[ Int, MuMap[ Int,  Double]] = {
    //println(" next from current ", crt)
    val grow  = MuMap[ Int, MuMap[ Int,  Double]]()
    
    //for(s1 <- crt.keys.flatMap{ case (s, _) => subsetExpanded(s)}){
    for(s1 <- crt.keys.flatMap{subsetExpanded}){
      grow(s1) = MuMap[Int, Double]().withDefault( c=>infinity)
      for( c1 <- subsetElements(s1)){
        val s0 = subsetRemoved(s1, c1)
        //println(" element " + s1 + "\t" + c1 + "\t" + s0)
        grow(s1)(c1)  = subsetElements(s0).map{ case c0 => crt(s0)(c0) + distance(c0)(c1)}.min
      }
    }
    grow.withDefault(c => MuMap[Int, Double]().withDefault(c => infinity))
  }
  

  //def solveFrom(c: Int) = ( Map( (1 << c, c) -> 0.0) /: (1 until N)){ case (m, _) =>  nxt(m)}
  //def solve = ( MuMap( (0 until N).map{case c =>  (1 << c)-> MuMap(c->0.0) }:_*) /: (1 until N)){ case (m, _) => nxt(m)}

  def solveFrom (sc: Int) = {
    //var grow = MuMap( (0 until N).map{case c =>  (1 << c)-> MuMap(c->0.0).withDefault(c=>infinity) }:_*).withDefault(c => MuMap[Int, Double]().withDefault(c => infinity))
    var grow  = MuMap( (1 << sc)->MuMap(sc->0.0).withDefault(c=>infinity)).withDefault(c => MuMap[Int, Double]().withDefault(c => infinity))
    for ( c <- 1 until N){ grow = nxt(grow)}
    grow.head._2.map{ case (ec, d) => d + distance(ec)(sc) }.min
  }


}
object PlaneTSP{
  def apply(cities: IndexedSeq[(Double, Double)]): PlaneTSP ={
    def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
      { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}
    new PlaneTSP((0 until cities.length).sortBy( c => distance(c, 0) ).map( i => cities(i)))
  }
}






class PlaneTSP( val cities: IndexedSeq[(Double, Double)]){
  val N = cities.length
  
  val distance: IndexedSeq[ IndexedSeq[Double]] = {
    for( i <- 0 until N) yield { 
         for(j <- 0 until N) yield { ( cities(i), cities(j)) match{
           case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
        }
      }
    }
  }
 
  val infinity = distance.flatten.sum

  def subsetElems(s: Int, i: Int = 0): List[Int] = {
    if (s == 0) List() 
    else {
      val t = subsetElems(s >> 1, i + 1)
      if ((s&1) == 0) t else i::t
    }
  }
  val oneElemSet: Set[Int] = (for( c <- 0 until N) yield (1 << c)).toSet

  def subsetContains(s: Int, c: Int) = ( s & ( 1 << c)) != 0
  def subsetRemoved(s: Int, c: Int) = if (subsetContains(s, c)) (s ^ ( 1 << c)) else s

  def solveFrom(cfirst: Int) : Double = {//Array[Array[Double]] = {
    val soltab = Array.tabulate[Double](1 << N, N){
                   (i, j) => (i, j) match {
                     case (0, _) => infinity
                     case (s , cfirst) if (s == (1 << cfirst)) => 0.0
                     //case (s, c) if (oneElemSet(s) && subsetElems(s)(0) == c) => 0.0
                     case _ => infinity
                   }
    }
    //def getMin(s: Int, c: Int): Double = for( l <- soltab(s) zip distance(c)) yield(_+_).min
   //now fill it
    //for(s <- (1 until (1 << N) ) if ( (s & (1 << c)) != 0); c <- (0 until N)) {
    //for(s <- ( 1 until 1 << N) ; c <- subsetElems(s)){
    for(s <- ( 1 until 1 << N) if (! oneElemSet(s)); c <- subsetElems(s) if c != cfirst){
      val s0 = s ^ (1 << c)
      soltab(s)(c) = (for(c0 <- subsetElems(s0)) yield (soltab(s0)(c0) + distance(c)(c0))).min
      println("subset " + s + ":\t"+ subsetElems(s), " city " + c + ":\t" +  soltab(s)(c))
      //soltab(s)(c) = (for ( (l, d) <- (soltab( s ^ ( 1 << c)).view zip distance(c).view) ) yield (l + d) ).min
      //soltab(s)(c) = ( soltab( subsetRemoved(s, c) ) zip distance(c)).map{ case (l, d) => l + d}.min
    }
    //add the last stop
    (for( (l, d)  <- (soltab.last.view zip distance(cfirst).view)) yield (l + d)).min
  }
}
object PlaneTSP{
  def apply(cities: IndexedSeq[(Double, Double)]): PlaneTSP ={
    def distance(i: Int, j: Int) = (cities(i), cities(j)) match 
      { case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1))}
    new PlaneTSP((0 until cities.length).sortBy( c => distance(c, 0) ).map( i => cities(i)))
  }
}


  
  def oneElemSet(s: Int): Boolean = (s&1, s >> 1) match {
    case (0, 0) => false
    case (1, 0) => true
    case (0, r) => oneElemSet(r)
    case (1, _) => false
  }
 
  

class PlaneTSP( val cities: IndexedSeq[(Double, Double)]){
  val N = cities.length - 1
  def computeDis(i: Int, j: Int): Double = (cities(i), cities(j)) match{
    case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
  }
 val distance0 = ( 0 until N).map( c => computeDis(0, c + 1))
 val distance: IndexedSeq[ IndexedSeq[Double]] = {
    for( i <- 0 until N) yield { 
         for(j <- 0 until N) yield { ( cities(i+1), cities(j+1)) match{
           case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
        }
      }
    }
  }
 
  val infinity = distance.flatten.sum

  def subsetElems(s: Int, i: Int = 0): List[Int] = {
    if (s == 0) List() 
    else {
      val t = subsetElems(s >> 1, i + 1)
      if ((s&1) == 0) t else i::t
    }
  }
  val oneElemSet: Set[Int] = (for( c <- 0 until N) yield (1 << c)).toSet

  def subsetContains(s: Int, c: Int) = ( s & ( 1 << c)) != 0
  def subsetRemoved(s: Int, c: Int) = if (subsetContains(s, c)) (s ^ ( 1 << c)) else s

  def solve : IndexedSeq[IndexedSeq[Double]] = {
    val soltab = Array.tabulate[Double](1 << N, N){
                   (i, j) => (i, j) match {
                     case (0, _) => infinity
                     //case (s , cfirst) if (s == (1 << cfirst)) => 0.0
                     case (s, c) if (oneElemSet(s) && subsetElems(s)(0) == c) => distance0(c)
                     case _ => infinity
                   }
    }
    //def getMin(s: Int, c: Int): Double = for( l <- soltab(s) zip distance(c)) yield(_+_).min
    for(s <- ( 1 until 1 << N) if (! oneElemSet(s)); c <- subsetElems(s)){
      val s0 = s ^ (1 << c)
      soltab(s)(c) = (for(c0 <- subsetElems(s0)) yield (soltab(s0)(c0) + distance(c)(c0))).min
      println("subset " + s + ":\t"+ subsetElems(s), " city " + c + ":\t" +  soltab(s)(c))
    }
    //add the last stop
    (for(xs <- soltab) yield IndexedSeq(xs:_*)).toIndexedSeq
 //   if ( N > 1) (for( (l, d)  <- (soltab.last.view zip distance0.tail.view)) yield (l + d)).min
   // else 2*distance0(0)
      
  }
}
object PlaneTSP{
  def apply(cities: IndexedSeq[(Double, Double)]): PlaneTSP = new PlaneTSP(cities)
}



class TSP( val distance: IndexedSeq[ IndexedSeq[ Double]]){
  val N = distance.length
  val infinity = distance.flatten.sum

  def subsetElems(s: Int, i: Int = 0): List[Int] = {
    if (s == 0) List() 
    else {
      val t = subsetElems(s >> 1, i + 1)
      if ((s&1) == 0) t else i::t
    }
  }
  val oneElemSet: Set[Int] = (for( c <- 0 until N) yield (1 << c)).toSet

  def subsetContains(s: Int, c: Int) = ( s & ( 1 << c)) != 0
  def subsetRemoved(s: Int, c: Int) = if (subsetContains(s, c)) (s ^ ( 1 << c)) else s

  def solveFrom(cstart: Int) : IndexedSeq[IndexedSeq[Double]] = {
    val soltab = Array.tabulate[Double](1 << N, N){
                   (i, j) => (i, j) match {
                     case (0, _) => infinity
                     case (s , cstart) if (s == (1 << cstart)) => 0.0
                     //case (s, c) if (oneElemSet(s) && subsetElems(s)(0) == c) => distance0(c)
                     case _ => infinity
                   }
    }
    for(s <- ( 1 until 1 << N) if (! oneElemSet(s)); c <- subsetElems(s) if (c != cstart)){
      val s0 = s ^ (1 << c)
      soltab(s)(c) = (for(c0 <- subsetElems(s0)) yield (soltab(s0)(c0) + distance(c)(c0))).min
      println("subset " + s + ":\t"+ subsetElems(s), " city " + c + ":\t" +  soltab(s)(c))
    }
    (for(xs <- soltab) yield IndexedSeq(xs:_*)).toIndexedSeq
  }

  def solveGivenEndPoints(cstart: Int, cend: int): Double = solveFrom( cstart)(cend)
  def solveWithPath(p: List[Int]): Double = TSP( for( i <- (0 until N) if (! p.contains(i)); j <- (0 until N) if (! p.contains(j))) yield distance(i, j))
}

object TSP{
  def apply(distance: IndexedSeq[ IndexedSeq[Double]]): TSP = new TSP(distance)
  def apply(cities: IndexedSeq[(Double, Double)]): TSP = {
    def computeDis(i: Int, j: Int): Double = (cities(i), cities(j)) match{
      case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
    }  
   val distance: IndexedSeq[ IndexedSeq[Double]] = {
      for( i <- 0 until cities.length) yield { 
          for(j <- 0 until cities.length) yield { ( cities(i), cities(j)) match{
            case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
          }  
        }
      }
    }
    new TSP(distance)
  }

  def groupPoints(points: Map[Int, (Double, Double)], ps: List[Int]): 
}

class PointOps( val loc: Map[Int, (Double, Double)]){
  def sep(i: Int, j: Int): Double = (loc(i), loc(j)) match{
    case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
  }  
  def pathLength(p: List[Int]): Double = p match{
    case Nil => 0.0
    case x::Nil => 0.0
    case x::y::us => sep(x, y) + pathLength(y::us)
  }

  def insidePath(x: Int, p: List[Int]): Boolean = !(p.contains(x)) || (x == p.head) || (x == p.last)

  def mergeSubPath(p: List[Int]):  Map[ Int, Map[ Int, Double]] = { 
    val distances: Map[Int, Map[Int, Double]] = Map(( for(i <- loc.keys.toSeq if (! insidePath(i, p))) yield 
       ( i, Map( (for(j <- loc.keys.toSeq if ( ! insidePath(j, p))) yield ( (j, sep(i, j) ))):_*))):_*)

    distances.updated( p.head, distances(p.head).updated(p.last,  pathLength(p))).updated(p.last, distances(p.last).updated(p.last, pathLength(p)))
  }
}

object PointOps{
  def apply( pm: Map[ Int, (Double, Double)]): PointOps = new PointOps(pm)
  def apply(ps: Map[Int, (Double, Double)], cs: List[Int]): PointOps = new PointOps( ps.filterKeys(cs.contains))
}
