package Collections.Tree


//note: January 23
//remove the return type Set[Any] from contents
//replace by [T] in the trait, and [Node] here.
//eventually we need this to be something that can be
//compared, which we have already implemented!
//we also need an == operator

class Node(private val value: Int) extends Containing[Node]
{
	def contents = Set(this.value)
	def ==(that: Node) = (this compare that) == 0
	def compare(that: Node) = this.value - that.value
	def max(that: Node): Node = if (this < that) that else this
	def min(that: Node): Node = if (this > that) that else this
	override def toString = value.toString
}

object Node {
	def apply(x: Int) = new Node(x)
}



