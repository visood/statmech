//my code that i have written while learning scala

package Collections

/* extending queue to a dequeue
 * from chapter 19.2, about type parameterization
 * this is a good useful example, implementing a functional
 * queue, while illustrating the use of type parameters
 
 * Type abstraction: hide initialization and representation of
 * of a class using traits. Make Queue a trait and not a class,
 * and implement the trait with a singleton object.
 */

//exporting only a trait to be used will hide representation
trait Dequeue[T] {
	def head: T
	def tail: Dequeue[T]
	def last: T
	def init:Dequeue[T]
	def ::(x: T): Dequeue[T]
	def append(x: T): Dequeue[T]
	def length : Int
}

object Dequeue {
	//apply () will hide initialization
	def apply[T] (xs: T*): Dequeue[T] = 
		new DequeueImpl[T] (xs.toList, Nil)
	
	private class DequeueImpl[T](
		private val leading: List[T],
		private val trailing: List[T]
	) extends Dequeue[T]
  {
		def mirrorTrail = 
			if (leading.isEmpty)
				new DequeueImpl(trailing.reverse, Nil)
			else
				this

		def mirrorHead = 
			if (trailing.isEmpty)
				new DequeueImpl(Nil, leading.reverse)
			else
				this
		
		def head: T = mirrorTrail.leading.head

		def last: T = mirrorHead.trailing.head
		
		def tail: DequeueImpl[T] = {
			val q = mirrorTrail
			new DequeueImpl(q.leading.tail, q.trailing)
		}

		def init: DequeueImpl[T] = {
			val q = mirrorHead
			new DequeueImpl(q.leading, q.trailing.tail)
		}

		def append(x: T) = 
			new DequeueImpl(leading, x::trailing)

		def ::(x:T) = new DequeueImpl(x::leading, trailing)

		def length: Int = leading.length + trailing.length
		//this toString will say Queue(List( . . .)). 
		//figure out a way to remove the word List 
		override def toString =
      "Dequeue(" + (leading ++ trailing.reverse).toString  + ")"

	}
}
	




