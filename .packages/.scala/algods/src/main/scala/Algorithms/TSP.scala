package Algorithms

class TSP(  val distMap: Map[ (Int, Int), Double] ){
  val infinity = distMap.values.sum
  val point = (for( (x, y) <- distMap.keys; z <- List(x, y)) yield z).toIndexedSeq
  val N = point.length
  //val pointAtIndex: Map[Int, Int] = Map( ( (0 until N) zip point):_*)
  val indexOfPoint: Map[Int, Int] = Map( (point zip (0 until N)):_*)

  val distance: IndexedSeq[ IndexedSeq[Double]] = 
   for(x <- point) yield (for( y <- point) yield distMap(x, y))
 

  def subsetElems(n: Int)(s: Int): List[Int] = 
    ( List[Int]() /: Range(n-1, -1, -1)){ case ( elems, i) => if ((s & ( 1 << i)) == 0) elems else i::elems}

  val oneElemSet: Set[Int] = (for( c <- 0 until N) yield (1 << c)).toSet

  def pathLength(p: List[Int]): Double = p match{
    case Nil => 0.0
    case x::Nil => 0.0
    case x::y::us => distMap((x,y)) + pathLength(y::us)
  }
  def insidePath(x: Int, p: List[Int]): Boolean = !(p.contains(x)) || (x == p.head) || (x == p.last)

  def subsetContains(s: Int)( cs: Int*) = cs.forall( c => ( s & ( 1 << c)) != 0)
  def subsetRemoved(s: Int, c: Int) = if (subsetContains(s)(c)) (s ^ ( 1 << c)) else s


  def subset(cs: Int*): Int = if (cs.isEmpty) 0 else subset(cs.tail:_*) | 1 << cs.head
  def subsetsExc(n: Int)(cs: Int*): Iterator[Int] = {
    def ssExc(m: Int, xs: List[Int]): (Iterator[Int], Int) = 
      (( Range(0,1).iterator, n) /: xs){ case ( (itr, m), c) => (itr.flatMap(b => Range(0, 1 << m - c, 2).iterator.map(_ << c).map(_|b)), c) }
    val (litr, cl) = ssExc(n, cs.toList.sorted(Ordering[Int].reverse))
    litr.flatMap( b => Range(0, 1 << cl).iterator.map(_|b))
  }
  def subsetsInc(n: Int)(cs: Int*): Iterator[Int] = subsetsExc(n)(cs:_*).map(_ | subset(cs:_*))

  def solveUntilLastStepFrom(pstart: Int) : IndexedSeq[ IndexedSeq[ Double ]] = {
    val cstart = indexOfPoint(pstart)
    val soltab = Array.tabulate[Double](1 << N, N){
                   (i, j) => (i, j) match {
                     case (0, _) => infinity
                     case (s , `cstart`) if (s == (1 << cstart)) => 0.0
                     case (s, c) if ( s == ( (1<< cstart) | (1 << c) )) => distance(cstart)(c)
                     case _ => infinity
                   }
    }
    for( s <- subsetsExc(N)(cstart).withFilter( !oneElemSet(_)); c <- subsetElems(N)(s)){
      val s0 = ( s ^ (1 << c)) 
      soltab(s | 1<< cstart)(c) = (for(c0 <- subsetElems(N)(s0)) yield (soltab(s0 | 1<< cstart)(c0) + distance(c0)(c))).min
      println("subset " + (s | 1<< cstart) + ":\t"+ point(cstart) + "--> " + subsetElems(N)(s0).map(point),  "--> " + point(c) + ":\t" +  soltab(s | 1<< cstart)(c))
    }
    println("-----------------------------")
    for( c <- Range(0, N).filter(_!=cstart)) println( point(cstart)+ "----------------->" + point(c) + ": " + (soltab.last(c) + distance(cstart)(c)) )
    (for(xs <- soltab) yield xs.toIndexedSeq).toIndexedSeq //cannot translate as the rows are subset indicies
  }

  def solveCircuitUntilLastStepFrom(pstart: Int): IndexedSeq[ IndexedSeq[ (List[Int], Double) ]] = {
    val cstart = indexOfPoint(pstart)
    val soltab = Array.tabulate[(List[Int], Double)](1 << N, N){
                   (i, j) => (i, j) match {
                     case (0, _) => (List[Int](), infinity)
                     case (s , `cstart`) if (s == (1 << cstart)) => (List(cstart), 0.0)
                     case (s, c) if ( s == ( (1<< cstart) | (1 << c) )) => (List(c), distance(cstart)(c))
                     case _ => (List[Int](), infinity)
                   }
    }

    val path = List(pstart)
    for( s <- subsetsExc(N)(cstart).withFilter( !oneElemSet(_)); c <- subsetElems(N)(s)){
      val s0 = ( s ^ (1 << c)) 
      soltab(s | 1<< cstart) (c) = (for(c0 <- subsetElems(N)(s0)) yield {
                                     val (p, l) = soltab(s0 | 1<< cstart)(c0)
                                     ( c::p , l + distance(c0)(c)) 
                                     }).minBy(_._2) 
    }
    (for(xs <- soltab) yield xs.toIndexedSeq).toIndexedSeq //cannot translate as the rows are subset indicies
  }

      
  def solveLengthFrom(pstart: Int): Double = 
    (solveUntilLastStepFrom(pstart).last zip distance( indexOfPoint(pstart))).map{ case (l, d) => l+d}.min
    
  def solveCircuitFrom(pstart: Int): (List[Int], Double) = 
    (solveCircuitUntilLastStepFrom(pstart).last zip distance(indexOfPoint(pstart))).map{ case ((p, l), d) => (p.reverse.map(point), l+d)}.minBy(_._2)


  def solveOpenEndedFrom(pstart: Int): Map[ Int, Double] = (point zip solveUntilLastStepFrom(pstart).last).toMap 

  def solveGivenEndPoints(pstart: Int, pend: Int): Double = solveOpenEndedFrom(pstart)(pend)

  def solveCircuitBetweenEndPoints(pstart: Int, pend: Int): (List[Int], Double) = 
    solveCircuitUntilLastStepFrom(pstart).last( indexOfPoint(pend) ) match { case (p, l) => (p.reverse.map(point), l)}

  def solveWithRequiredEdgeStartingFrom(i: Int, j: Int, pstart: Int): IndexedSeq[IndexedSeq[Double]] = {
    val ci = indexOfPoint(i)
    val cj = indexOfPoint(j)
    val cstart = indexOfPoint(pstart)
    val soltab = Array.tabulate[Double](1 << N, N){
                  (i, j) => (i, j) match {
                     case (0, _) => infinity
                     case (s, cstart) if ( s == (1 << cstart) ) => 0.0
                     case _ => infinity
                  }
    }
    for( s <- (1 until (1 << N)) if ( (! oneElemSet(s) && (subsetContains(s)(cstart))))){
      if ( ! subsetContains(s)(ci, cj) ){
        for( c <- subsetElems(N)(s) if (c != cstart)){
          val s0 = s ^ ( 1 << c)
          soltab(s)(c) = ( for(c0 <- subsetElems(N)(s0)) yield ( soltab(s0)(c0) + distance(c0)(c))).min
        }
       }
      else {
        for(c <- subsetElems(N)(s) if (c != cstart)) { 
          val s0 = s ^ (1 << c)
          soltab(s)(c) = c match{
            case `ci` => soltab(s0)(cj) + distance(cj)(ci)
            case `cj` => soltab(s0)(ci) + distance(ci)(cj)
            case _    => ( for(c0 <- subsetElems(N)(s0)) yield ( soltab(s0)(c0) + distance(c0)(c))) .min 
          }
        }
      }
    }
    (for(xs <- soltab) yield xs.toIndexedSeq).toIndexedSeq //cannot translate as the rows are subset indicies
   }

  def solveWithPath(p: List[Int]): Double = {
    val unspecified = TSP( distMap.filterKeys{ case (x, y) => !( insidePath(x, p) || insidePath(y, p))})
    unspecified.solveGivenEndPoints(p.head, p.last) + pathLength(p)
  }
  

  def solveForEndPointFrom(pstart: Int) = 
    ((for( (l, d) <- solveUntilLastStepFrom(pstart).last zip distance( indexOfPoint(pstart) )) yield(l + d)) zip point).minBy(_._1)

  def subProblemOver(cs: Int*) = new TSP( distMap.filterKeys{ case (x, y) => cs.contains(x) && cs.contains(y)} )
  def subProblemExcluding(cs: Int*) = new TSP ( distMap.filterKeys{case (x, y) => ! (cs.contains(x) || cs.contains(y)) } )

  //fish steps on the path
  def stepsInOpenEndedSol(x: Int, openEnded: IndexedSeq[Double]) = {
    val oeMapped = (point zip openEnded).toMap
    ((for( y <- point) yield ( (y, oeMapped(y) + distMap(x, y) ) )).sortBy(_._2) take 2)
  }

}

object TSP{
  def apply(disM: Map[ (Int, Int), Double]): TSP = new TSP(disM)

  def apply(cities: IndexedSeq[(Double, Double)]): TSP = {
    def computeDis(i: Int, j: Int): Double = (cities(i), cities(j)) match{
      case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
    }  
    def separation(i: Int, j: Int): Double =  (cities(i), cities(j)) match {
      case ( (x0, y0), (x1, y1) ) => math.sqrt( (x0 -x1)*(x0-x1) + (y0 - y1)*(y0 - y1)) 
    } 
    val distance: IndexedSeq[ IndexedSeq[Double]] = {
       val cs = 0 until cities.length
       cs.map(i => cs.map( j => separation(i, j)))
    }
    new TSP( ( for( i <- 0 until distance.length; 
                    j <- 0 until distance(i).length )
                    yield ((i, j), distance(i)(j)) ).toMap
           )
  }
}

