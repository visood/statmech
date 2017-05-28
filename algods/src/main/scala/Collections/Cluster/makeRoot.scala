package Collections.Tree

object makeRoot
{
	def groupTwo[T](forks: List[Cluster[T]]): List[Cluster[T]] = {
		if (forks.size == 1) List(forks.head)
		else mergeTrees(forks.head, forks.tail.head) :: forks.tail.tail
	}
	def apply[T](forks: List[Cluster[T]]): Cluster[T] = {
		if (forks.size == 1) forks.head
		else makeRoot(groupTwo(forks))
	}
}

