//my code that i have written while learning scala

package Collections

/*
 * from chapter 19.2, about type parameterization
 * this is a good useful example, implementing a functional
 * queue, while illustrating the use of type parameters
 
 * Type abstraction: hide initialization and representation of
 * of a class using traits. Make Queue a trait and not a class,
 * and implement the trait with a singleton object.
 */

//exporting only a trait to be used will hide representation
//notice some state modifiers have creeped in
trait Queue[+T] {
	def head: T
	def tail: Queue[T]
	def append[U >: T] (x: U): Queue[U]
	def length : Int
}

object Queue {
	//apply () will hide initialization
	def apply[T] (xs: T*): Queue[T] = 
		new QueueImpl[T] (xs.toList, Nil)
	
	private class QueueImpl[T](
		var leading: List[T],
		var trailing: List[T]
	) extends Queue[T] {
		
		private def mirror() = 
			if (leading.isEmpty){
				while(!trailing.isEmpty) {
					leading = trailing.head :: leading
					trailing = trailing.tail
				}
			}
		
		def head: T = {
			mirror()
			leading.head
		}
		
		def tail: QueueImpl[T] = {
			mirror()
			new QueueImpl(leading.tail, trailing)
		}

		def append[U>:T](x: U) = 
			new QueueImpl[U](leading, x::trailing)

		def length: Int = leading.length + trailing.length
		//this toString will say Queue(List( . . .)). 
		//figure out a way to remove the word List 
		override def toString =
      "Queue(" + (leading ++ trailing.reverse).toString  + ")"

	}
}
	



