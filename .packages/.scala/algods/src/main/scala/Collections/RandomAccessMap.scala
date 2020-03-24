/* Data structures,
 * here, a map that can return random elements
*/

package Collections
import scala.language.postfixOps
import scala.language.implicitConversions


//the first version wraps a map with a vector for indexing and another (inverse) map from keys to the index
/*

class RandomAccessMap[S, +T]( val elems: Map[S, T], val keyVec: IndexedSeq[S], val keyIndex: Map[S, Int]) extends Map[S, T]{

  //def apply(s: S): T = elems(s)

  //def getOrElse(s: S, t: T): T = elems.getOrElse(s, t)

  //def length: Int = keyVec.length

  def get(s: S): Option[T] = elems.get(s)

  def iterator: Iterator[(S, T)] = elems.iterator


  override def default(s: S): T = elems.default(s)

  def + [T1 >: T](kv: (S, T1)): RandomAccessMap[S, T1] = {
    if (elems.isDefinedAt(kv._1)) new RandomAccessMap(elems + kv, keyVec, keyIndex)
    else new RandomAccessMap( elems + kv, keyVec :+ kv._1, keyIndex + (kv._1 -> keyVec.length))
  }

  def -(s: S): RandomAccessMap[S, T] = {
    val newKeys = keyVec.updated(keyIndex(s), keyVec.last).init
    val newIndex = keyIndex.updated(keyVec.last, keyIndex(s)) - s
    new RandomAccessMap(elems - s, newKeys, newIndex).withDefault(this.default(_))
  }

  def randomElement(implicit rg: util.Random): (S, T) = {
    val s = randomKey
    (s, elems(s))
  }
 
  def randomKey(implicit rg: util.Random): S = keyVec(rg.nextInt(keyVec.length))

  def randomValue(implicit rg: util.Random): T = elems(randomKey)


  override def withDefault[T1 >: T](df: S => T1): RandomAccessMap[S, T1] =
    new RandomAccessMap(elems.withDefault(df), keyVec, keyIndex)

  override def withDefaultValue[T1 >: T](d: T1): RandomAccessMap[S, T1] =
    new RandomAccessMap(elems.withDefaultValue(d), keyVec, keyIndex)

}
    
object RandomAccessMap{
  def apply[S, T](m: Map[S, T]): RandomAccessMap[S, T] = {
    val kvec = m.keys.toIndexedSeq
    val kidx = for(i <- 0 until kvec.length) yield (kvec(i), i)

    new RandomAccessMap(m, kvec, kidx.toMap)
  }
}
*/


//we dont need a map inside RAM
class RandomAccessMap[S, +T](val keyVec: IndexedSeq[(S, T)], val keyIndex: Map[S, Int]) extends Map[S, T]{

  override def toString = "RandomAccess" + super.toString

  def get(s: S): Option[T] = keyIndex.get(s) map ( (i: Int) => keyVec(i)._2)

  def iterator: Iterator[ (S, T) ] =  {
    (keyIndex.iterator) map ( (st: (S, Int)) => keyVec(st._2))
  }

  def + [T1 >: T](kv: (S, T1)): RandomAccessMap[S, T1] = {
    if (keyIndex.isDefinedAt(kv._1)) new RandomAccessMap(keyVec.updated(keyIndex(kv._1), kv), keyIndex)
    else new RandomAccessMap(keyVec :+ kv, keyIndex + (kv._1 -> keyVec.length))
  }

  def - (s: S): RandomAccessMap[S, T] = {
    val newKeys = keyVec.updated(keyIndex(s), keyVec.last).init
    val newIndex = keyIndex.updated(keyVec.last._1, keyIndex(s)) - s
    new RandomAccessMap(newKeys, newIndex)
  }
  

  def randomElement(implicit rg: util.Random): (S, T) = keyVec( rg.nextInt(keyVec.length))
  def randomKey(implicit rg: util.Random): S = randomElement._1
  def randomValue(implicit rg: util.Random): T = randomElement._2

  override def withDefault[T1 >: T](df: S => T1): RandomAccessMap[S, T1] = new RandomAccessMap.WithDefault(this, df)

  override def withDefaultValue[T1 >: T](d: T1): RandomAccessMap[S, T1] = new RandomAccessMap.WithDefault(this, (s: S) => d)

}
    
  
object RandomAccessMap{
  def apply[S, T](m: Map[S, T]): RandomAccessMap[S, T] = {
    val kvec = m.toIndexedSeq
    val kidx = for(i <- 0 until kvec.length) yield (kvec(i)._1, i)

    new RandomAccessMap(kvec, kidx.toMap)
  }

  class WithDefault[S, T](underlying: RandomAccessMap[S, T], df: S=>T) extends RandomAccessMap[S, T](underlying.keyVec, underlying.keyIndex){
    override def default(s: S) = df(s)
  }
}



