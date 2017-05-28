package graph.generators
import scala.util.Random._
import breeze.stats.distributions._


	/** 
		* An implementation of The Markov Chain Simulation Method for Generating Connected
		* Power Law Random Graph
		*/
object Gkantsidis extends App{


  import Graph._

  def apply(degSeq: Seq[Int]): Option[AL] = {
    //for( links <- realized( (vs: IndexedSeq[Vertex]) => randomIdx(vs))(degSeq) )
    for {  links <- realized( randomIdx )(degSeq); 
           al <- connected(adjListForLinks(links)) 
        } yield mcmc(al)(linksInAdjList(al).length)
    /*
    realized( (vs: IndexedSeq[Vertex]) => randomIdx(vs)) (degSeq) match{
      case None => None
      case Some(links) => 
      //case Some(links) => mcmcd( connected( adjListForLinks(links) ) )
    }
    */
  } 
	


  case class Vertex(val label: Int, val degree: Int)
  
  implicit object DegOrdering extends Ordering[Vertex] {
    def compare(v: Vertex, u: Vertex) = -(v.degree compare u.degree)
  }

  def randomIdx(vs: IndexedSeq[Vertex]): Option[Int] = vs.length match{
    case 0 => None
    case n => Some(nextInt(n))
  }

  def realized(idx: IndexedSeq[Vertex] => Option[Int])(degSeq: Seq[Int]):
      Option[ IndexedSeq[(Int, Int)]] = {
    //this algorithm is not correct, or I miss-interpreted
    def linked(vs: IndexedSeq[Vertex], k: Int): IndexedSeq[Vertex] = 
      vs.take(k).map{
        case Vertex(l, d) => Vertex(l, d - 1)
      }.filter{case Vertex(_, d1) => d1 > 0} ++ vs.drop(k)

    def rlzdFrom( vs: IndexedSeq[Vertex], links: IndexedSeq[(Int, Int)]):
        Option[IndexedSeq[(Int, Int)]] = {
      idx(vs) match{
        case None => Some(links)
        case Some(i) => {
          val v = vs(i)
          val rest = vs.take(i) ++ vs.drop(i+1)
          if (rest.length < v.degree) None
          else rlzdFrom( linked(rest, v.degree).sorted,
                         links ++ rest.take(v.degree).map{
                           case Vertex(l, _) => (l, v.label)
                         })
        }
      }
    }
    rlzdFrom( (Range(0, degSeq.length).map {
                 case i => Vertex(i, degSeq(i))
               }).sorted, IndexedSeq[(Int, Int)]() )
  }


  def edgesToSwap(al1: AL, al2: AL): Option[((Int, Int), (Int, Int))] = {
    (
      for{ e1 <- randomLinkOnCycle(al1);
           e2 <- randomLink(al2)
      } yield (e1, e2)
    ) match{
      case None => (
        for{e1 <- randomLink(al1);
            e2 <- randomLinkOnCycle(al2)
        } yield (e1, e2)
      )
      case Some(e12) => Some(e12)
    }
  }


  def connectTwo(al1: AL, al2: AL): Option[AL] = edgesToSwap(al1, al2) match {
    case None => None
    case Some( (e1, e2) ) => edgeSwap(e1, e2) match {
      case (f1, f2) => Some(linksAdded(linksRmvd(al1)(e1) ++ linksRmvd(al2)(e2))(f1, f2))
    }
  }
  
  def twoEdgesSwapped(al: AL): Option[AL] = connectTwo(al, al)

  def connected( al: AL): Option[AL] = components(al).sortBy{
    case c => - numLinksOnCycle( projection(al)(c) )
  }  match {
    case Nil        =>  None
    case c1 :: cmps =>  ( (Some(projection(al)(c1)): Option[AL]) /: cmps) {
      case (None, _) => None;
      case (Some(al1), c) => connectTwo(al1, projection(al)(c))
    }
  }


  def mcmc(al: AL)(T: Int): AL = {
    def step(alx: AL): AL = twoEdgesSwapped(alx) match {
      case None => println("no edges on loops"); alx
      case Some(aly) => aly
    }
    (al /: Range(0, T)){ case (alx, _) => step(alx)}
  }


	object tests {
		def cmptOverlap( al: AL): Boolean = {
			val cs = components(al)
			if (
        (
          for{b <- cs;
              c <- cs;
              if (c != b)
          } yield ( b.intersect(c).size)
        ).sum == 0
      ) true
      else false
		}

    def sameAdjLists(al1: AL, al2: AL): Boolean = {
      if (al1.keys != al2.keys) false
      else { al1.find{ case (x, y) => al2(x) != y} match {
              case None => true;
              case Some(_) => false
            }
      }
    }

    def sameDegrees( ds1: Map[Int, Int], ds2: Map[Int, Int]): Boolean = {
      if (ds1.keys != ds2.keys) false
      else { ds1.find{ case (x, k) => ds2(x) != k} match {
              case None => true; case Some(_) => false
            }
      }
    }

    def cnctdVtxOverlap( al: AL, c1: Set[Int], c2: Set[Int]): Boolean = {
      val al1 = projection(al)(c1)
      val al2 = projection(al)(c2)
      connectTwo(al1, al2) match {
        case None       => println(" no links that could connect "); true
        case Some(al12) => (
          vertexes(al1) ++ vertexes(al2)
        ).toSet == vertexes(al12).toSet
      }
    }
  
    //may be this test should be in Graph
		def cnctdDegSeq(al: AL, c1: Set[Int], c2: Set[Int]): Boolean = {
			val al1 = projection(al)(c1)
			val al2 = projection(al)(c2)
      connectTwo(al1, al2) match {
        case None       =>  println(" no links that could connect "); true
        case Some(al12) =>  sameDegrees(degrees(al1) ++ degrees(al2),
                                        degrees(al12) )
      }
      
    }
	}

}

