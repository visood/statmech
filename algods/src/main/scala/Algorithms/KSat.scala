//using strongly connected componentsQ
package Algorithms


object twosat{
  def validCluster(termSet: Set[Int]): Boolean = {
    val negs = termSet.filter(_<0)
    val poss = termSet.filter(_>0)
    (negs.map(-_) intersect poss).isEmpty
  }

  def satisfiable(clauses: List[(Int, Int)]): Boolean = {
    val implg = Implication(clauses)
    val olAS = implg.remOutDegZero(implg.adjSet)
    val ilolAS = implg.remInDegZero(olAS)
    SCC(ilolAS).clusters.forall( validCluster)
  }
}


class SCC( val adjSet: Map[Int, Set[Int]])  {
  lazy val vertices = adjSet.flatMap{ case (i, s) => s + i}.toSet
  
  def reachableFrom(x: Int): Set[Int] = 
    ( Set(x) /: adjSet(x)){ case (s, y) => s ++ reachableFrom(y)}.toSet

  def clustersFrom(x: Int): List[Set[Int]] = {
    def followTrace(y: Int, isUpStream: Set[Int], cs: Map[Int, Int]): Map[Int, Int] = {
      def downStream(scn: Map[Int, Int], u: Int): Map[Int, Int] = {
        if (isUpStream( scn.getOrElse(u, u) )) scn + (y -> cs.getOrElse(u, u)) 
        else {
          val down = followTrace(u, isUpStream + u, scn)
          if (isUpStream(down(u))) down + (y -> down(u)) else down
        }
      }
      val dnstrm = ( (cs + (y -> y)) /: adjSet(y))(downStream)
      dnstrm
    }
    val scm = followTrace(x, Set(x), Map[Int, Int]())
    scm.groupBy(_._2).map{ case (i, m) => (i, m.keys)}.values.toList.map(_.toSet)
  }     

  def clusters: List[Set[Int]] = {
    val sc0 = clustersFrom( adjSet.head._1)
    val rem = vertices -- sc0.toSet.flatten
    if (rem.isEmpty) sc0
    else sc0 ++ SCC( adjSet.filterKeys( rem ).map{ case (v, s) => (v, s.intersect( rem ) )} ).clusters
  }
}
      
object SCC{
  def apply(al: Map[ Int, Set[Int]]): SCC = new SCC(al)
}

class DirectedComponents(val adjSet: Map[Int, Set[Int]]){
  def expand(core: Set[Int], frtr: Set[Int]): (Set[Int], Set[Int]) = {
    ((core, Set[Int]())  /: frtr){ case ((c, f), x) => if (!c(x))( c + x, f ++ adjSet(x)) else (c, f)}
  }
  def reachableFrom(x: Int): Set[Int] = {
    def grow(core: Set[Int], frtr: Set[Int]): Set[Int] = if (frtr.isEmpty) core else (grow _).tupled( expand(core, frtr))
    grow( Set[Int](), Set(x)) 
  }
  lazy val clusters: List[ Set[Int] ] = {
    def accumulate(vs: Set[Int], sets: List[ Set[Int]]): List[ Set[ Int] ] = 
      if (vs.isEmpty) sets 
      else {
        val s = reachableFrom(vs.head) 
        accumulate(vs diff s, s::sets)
      }
    accumulate(adjSet.flatMap{ case (i, s) => s + i}.toSet, List[Set[Int]]())
  }
}
object DirectedComponents{
  def apply(adjSet: Map[Int, Set[Int]]): DirectedComponents = new DirectedComponents(adjSet)
}

class Implication(val clauses: List[ (Int, Int) ]){
  import scala.math.abs
  lazy val terms: Set[Int] = clauses.flatMap{ case (i, j) => List(abs(i), abs(j))}.toSet
  lazy val edges = clauses.flatMap{ case (i, j) => List( (-i, j), (-j, i) ) }
  lazy val adjList: Map[Int, List[Int]] =
    edges.groupBy(_._1).map{ case (v, l) => (v, l.map(_._2))}.withDefault(i=>Nil)
  val adjSet: Map[Int, Set[Int]] = 
    edges.groupBy(_._1).map{ case (v, l) => (v, l.map(_._2).toSet)}.withDefault(i=>Set.empty)

  def remOutDegZero(AS: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    val outLinked = AS.keys.toSet
    val nAS = AS.map{ case (v, s) => (v, s.intersect( outLinked) ) }.filter(! _._2.isEmpty)
    if (nAS.size == AS.size) nAS else remOutDegZero(nAS)
  }
  
  def remInDegZero(AS: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    val inlinked = AS.values.flatten.toSet
    val nAS = AS.filterKeys(inlinked).map{ case (v, s) => (v, s.intersect( inlinked ) )}
    if (nAS.size == AS.size) nAS else remInDegZero(nAS)
  }
}

object Implication{
  def apply(clauses: List[ (Int, Int) ]): Implication = new Implication(clauses)
}

def next(xs: Set[Int], core: Set[Int]): (Set[Int], Set[Int]) = {
  val nxs = xs.flatMap{case x => AS(x).filter(!core(_))}
  (nxs, core ++ nxs)
}

def deadEnds(xs: Set[Int], core: Set[Int], dends: Set[Int]): Set[Int] = {
  if (xs.isEmpty) dends
  else{ 
    val (nxs, ncr) = next(xs, core)
    deadEnds(nxs, ncr, dends ++ xs.filter(x => AS(x).isEmpty))
  }
}
  

def tillNoMore( xs: Set[Int], core: Set[Int]): Set[Int] = {
  val (nxs, ncr) = next(xs, core)
  if (!nxs.isEmpty) tillNoMore(nxs, ncr) else ncr
}

       
      

val clauses = io.Source.fromFile(fn).getLines.toList.tail.map{ line => line.split(' ').map(_.toInt)}.map{ case Array(i, j) => (i, j)}

