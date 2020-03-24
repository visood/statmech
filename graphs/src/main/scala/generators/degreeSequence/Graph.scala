package graph.generators
import scala.util.Random._

object Graph{
  type AL = Map[Int, IndexedSeq[Int]]
  type AS = Map[Int, Set[Int]]

	def vertexes(al: AL): IndexedSeq[Int] =
      (al.keys ++ al.values.flatten).toIndexedSeq

	def vertexes(links: IndexedSeq[(Int, Int)]): IndexedSeq[Int] =
      (for( (x, y) <- links) yield List(x,y)).flatten.distinct.sorted

  def degrees(al: AL): Map[Int, Int] = al.map {
    case (x, nx) => (x, nx.length)
  }.toMap

	def emptyAL: AL = Map[Int, IndexedSeq[Int]]().withDefault(
      (i: Int) => IndexedSeq[Int] ()
  )

  def nonbrAL(vs: IndexedSeq[Int]): AL =
    ( (for(v <- vs) yield(v, IndexedSeq[Int]())) ).toMap

  def makeLink(x: Int, y: Int): (Int, Int) =
    if (x < y) (x, y) else (y, x) //what is x == y?
	
  def linksAdded(al: AL)( links: (Int, Int)*): AL =
	(al /: links) {
      case (al1, (x, y)) => al1 ++ Seq( x -> (al1(x) :+ y),
        y -> (al1(y) :+ x))
    }

  def adjListForLinks(links: IndexedSeq[(Int, Int)]): AL =
    linksAdded(nonbrAL( vertexes(links))) ( links:_*)

  def linksRmvd(al: AL)( links: (Int, Int)*): AL = {

    (al /: links) {
      case (al1, (x, y)) => al1 ++ Seq(x -> al1(x).filter(_!= y),
                                       y -> al1(y).filter(_!= x))
    }
  }


	def linksERRGgivenProb( N: Int, p: Double): IndexedSeq[ (Int, Int) ] = 
		for { i <- 0 to N;
          j <- i + 1 until N;
          if (nextDouble < p) } yield makeLink(i, j)
    

	def adjListERRGgivenProb( N: Int, p: Double): AL =
    linksAdded( nonbrAL(Range(0, N)) ) (linksERRGgivenProb(N, p): _*)

  def linksERRGgivenNumber(N: Int, M: Int): IndexedSeq[ (Int, Int)] = {
    val allLinks = for{ x <- Range(0, N);
                        y <- Range(x+1, N) } yield (x, y)
    shuffle(allLinks).take(M)//simple, straightforward, could be made faster
  }

  def adjListERRGgivenNumber(N: Int, M: Int): AL =
    linksAdded( nonbrAL(Range(0, N)) ) (linksERRGgivenNumber(N, M): _*)
	
	def linksInAdjList(al: AL): IndexedSeq[(Int, Int)] = (Set[(Int, Int)]() /: al){
      case (links, (x, nxs)) => links ++ nxs.map(makeLink(x, _))
    }.toIndexedSeq
	

  trait GraphTraversal
  {
		//def nbrs( x: Int): List[Int]
		def nbrs: Int => List[Int]
		def insert(zs: List[Int])(ys: Int*): List[Int]
		def updated(qd: Set[Int], ys: Set[Int]): Set[Int] = qd ++ ys

		def grow(xs: List[Int], qd: Set[Int]): List[Int] = xs match {
			case Nil => Nil
			case x :: zs => { val ys = nbrs(x).filter( ! qd(_) );
					x :: grow( insert(zs)(ys:_*), updated( qd, ys.toSet))
			}
		}
		def from(x: Int): List[Int] = grow( List(x), Set(x))
	}

  class BreadthFirstTraversal(val nbrs: Int => List[Int]) extends GraphTraversal
  {
		def insert(zs: List[Int])( ys: Int*): List[Int] = zs ++ ys

		def notPathed(xs: List[Int],  paths: Map[Int, List[Int]]): List[Int] =
      xs.filter( ! paths.isDefinedAt(_))

		def shortestPathsFrom(x: Int): Map[Int, List[Int]] = {

			def extnd(path: List[Int], ys: List[Int]): Map[Int, List[Int]] =
        ys.zip(ys.map(path :+ _)).toMap

			def collect(xs: List[Int],
                  paths: Map[Int, List[Int]]): Map[Int, List[Int]] =
        xs match{
					case Nil => paths
					case x :: ys => collect(
            ys, paths ++ extnd(paths(x),
                               notPathed(nbrs(x), paths).filter(ys.contains(_))
            )
          )
			}
			collect(this.from(x).tail, Map(x -> List(x)))
		}
	}
  object BreadthFirstTraversal {
    def apply(nbrs: Int => List[Int]): BreadthFirstTraversal =
      new BreadthFirstTraversal(nbrs)
    def apply(adjlist: AL): BreadthFirstTraversal =
      apply( (x: Int) => adjlist(x).toList)
  }

  class DepthFirstTraversal( val nbrs: Int => List[Int]) extends GraphTraversal
  {
		def insert(zs: List[Int])(ys: Int*): List[Int] = ys ++: zs

    def clsdNbrs(x: Int, open: Set[Int]): List[Int] = nbrs(x).filter( ! open(_))

    def allPathsFromAvoiding(x: Int, open: Set[Int]): List[ List[Int]] =
      clsdNbrs(x, open) match {
        case Nil => List(List(x))
        case ys => for{
          y <- ys;
          sy <- allPathsFromAvoiding( y, open + x)
        } yield (x :: sy)
    }

		def allPathsFrom(x: Int): List[List[Int]] =
      allPathsFromAvoiding(x, Set[Int]())


    def subPathUpto( path: List[Int], x : Int): List[Int] = path match{
      case Nil => Nil
      case y :: ys =>	{ if (y == x) x :: List[Int]()
                        else subPathUpto(ys, x) match {
                               case Nil => Nil
                               case p =>  y :: p
                             }
                      }
		}

		def dftWithSink(x: Int): DepthFirstTraversal = 
			DepthFirstTraversal( (z: Int) => if (z == x) List[Int]() else nbrs(z))
			
		def dftLinkRmvd(x: Int, y: Int): DepthFirstTraversal = {
				DepthFirstTraversal( (z: Int) => 	z match{ 
                                            case `x` => nbrs(z).filter(_ != y)
                                            case `y` => nbrs(z).filter(_ != x)
                                            case _ => nbrs(z)
																				  }
													)
    }  

		def allPathsBetween(x: Int, y: Int): List[List[Int]] = {
			val snkd = dftWithSink(y)
      snkd.allPathsFrom(x).map( snkd.subPathUpto(_, y) ).filter( _ != Nil )
		}

	}
	object DepthFirstTraversal {
    def apply(nbrs: Int => List[Int]): DepthFirstTraversal =
      new DepthFirstTraversal(nbrs)

    def apply(adjlist: AL): DepthFirstTraversal =
      apply( (x: Int) => adjlist(x).toList)
  }


	def edgeSwap( e: (Int, Int), f: (Int, Int)): ((Int, Int), (Int, Int)) = 
		( makeLink(e._1, f._1), makeLink(e._2, f._2) )
		
	def areCnctd(al: AL)(x: Int, y: Int): Boolean =
    DepthFirstTraversal(al).dftWithSink(y).from(x).contains(y)


  def randomLink(al: AL): Option[(Int, Int)] = linksInAdjList(al) match{
    case IndexedSeq() => None
    case links => Some( links( nextInt(links.length) ) )
  }


  def onCycle(al: AL)(xy: (Int, Int)): Boolean = xy match
    { case(x, y) => areCnctd( linksRmvd(al)((x, y)) )(x, y)}

  def firstLinkOnCycle(al: AL): Option[(Int, Int)] =
    linksInAdjList(al).find( onCycle(al) )

  def randomLinkOnCycle(al: AL): Option[(Int, Int)] =
    linksOnCycle(al) match{
      case IndexedSeq() => None
      case ls => Some(ls( nextInt(ls.length) ))
  }

  def hasCycle(al: AL): Boolean =
    linksInAdjList(al).find(onCycle(al)) match {
      case None => false
      case Some(_) => true
  }

  def linksOnCycle(al: AL): IndexedSeq[(Int, Int)] = 
    if (al.isEmpty) IndexedSeq[ (Int, Int) ]()
    else linksInAdjList(al).filter(onCycle(al))

  def numLinksOnCycle(al: AL): Int = linksOnCycle(al).length

 //define a component as a set of Ints
  //write a test to check if the DepthFirstTraversal.from produces a list of distinct elements
	def vtxCmpnt(al: AL)( x: Int): Set[Int] =
    DepthFirstTraversal(al).from(x).toSet

	def addCmpnt(c: Set[Int], cs: List[Set[Int]], mkd: Set[Int]): List[Set[Int]] =
  {
    if (c.isEmpty) cs
    else { if (mkd(c.head)) cs else (c :: cs) }
  }

	def components(al: AL): List[Set[Int]] = {
		val dftr = DepthFirstTraversal(al)
		( (List[Set[Int]](), Set[Int]()) /: vertexes(al) ) {
			 case ((cs, mkd), x) =>	if (mkd(x)) (cs, mkd) 
															else {
                                val c = dftr.from(x).toSet;
                                (c :: cs, mkd ++ c)
                              }
		}._1
	}

  def largestCmpnt(al: AL): Set[Int] = components(al).maxBy( _.size)

		

  def projection(al: AL)(c: Set[Int]): AL = c.map {
    case x => (x, al(x).filter( c(_) ) )
  }.toMap
    
}


object GraphTests{
	import Graph._
	
	def isSymmetric(al: AL): Boolean = 
		(for{ (x, nxs) <- al;
          y <- nxs} yield (x, y)
    ).forall{ case (u, v) => al(v).contains(u)}
}
	

    
    


                              


    

