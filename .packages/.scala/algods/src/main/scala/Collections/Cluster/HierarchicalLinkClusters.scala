package Collections.Tree


class Link(x: Int, y: Int) extends Containing[Link]
{
	require(x != y)
	private val head = x min y
	private val tail = x max y
	def contents = Set(this.head, this.tail)
	def ==(that: Link) = (this compare that) == 0

	def compare(that: Link) =
		if (that.head != this.head) this.head - that.head
    else this.tail - that.tail

	def max(that: Link): Link =  if (this < that) that else this
	def min(that: Link): Link =  if (this > that) that else this

	override def toString = head.toString +  "~" + tail.toString
}

object Link
{
	def apply(x: Int, y: Int) = new Link(x, y)
}

