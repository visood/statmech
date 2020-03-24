package Collections
//import scala.collection.immutable.TreeMap
import scala.language.postfixOps
import scala.language.implicitConversions

class RandomAccessSet[T](val keyVec: IndexedSeq[T],
                         val keyIndex: Map[T, Int]) extends Set[T]
{

  type This = RandomAccessSet[T]
  
  override def toString = "RandomAccessSet containing " + keyVec.toString

  def apply(i: Int) = keyVec( i )

  def length = keyVec.length

  def contains(key: T): Boolean = keyIndex.contains(key)

  def iterator: Iterator[T] = keyIndex.iterator map ( (ti: (T, Int)) => ti._1)

  def +(elem: T): This = 
    if (contains(elem)) this
    else new RandomAccessSet(keyVec :+ elem, keyIndex + (elem -> keyVec.length))

  def - (elem: T): This  = 
    if (contains(elem))
      new RandomAccessSet(keyVec.updated( keyIndex(elem), keyVec.last).init,
                          (keyIndex - elem) + (keyVec.last -> keyIndex(elem)))
    else this
  
  def randomElement(implicit rg: util.Random): T =
    keyVec(rg.nextInt(keyVec.length))

  override def head: T = keyVec head

  override def last: T = keyVec last

  override def tail: This = RandomAccessSet(keyVec tail)
  
  override def init: This = RandomAccessSet(keyVec init)
}



object RandomAccessSet{
  def apply[T](v: IndexedSeq[T]): RandomAccessSet[T] = {
    val idx = for (i <- 0 until v.length) yield (v(i), i)
    new RandomAccessSet(v, Map(idx: _*))
  }

  def apply[T](s: Set[T]): RandomAccessSet[T] = apply(s.toIndexedSeq) 
  
  def apply[T](xs: T*): RandomAccessSet[T] = apply(xs.toIndexedSeq)
    
}


  
