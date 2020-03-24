package Collections.Matrix

/*
 * an attempted implementation of a matrix
 * 2013 February 04, Bussigny Monday Evening
 */


class Matrix[T]( val elems: Vector[Vector[T]])
{
	val rows = elems.size
	val cols = elems(0).size
	override def toString = "matrix " + rows + " BY " + cols

	def updated( i: Int, j: Int, newValue: T) = {
		val newRow =  elems(i).updated(j, newValue)
		new Matrix( elems.updated(i, newRow))
	}	

	def rowadded( newrow: Vector[T]) = {
		require( newrow.size == cols)
		new Matrix(elems :+ newrow)
	}
	
	
	def coladded( newcol: Vector[T]) = {
		require( newcol.size == rows)
		val newelems  = for(i <- (0 until rows).toList) yield elems(i) :+ newcol(i)

		new Matrix(Vector(newelems: _*))
	}

	def apply(i: Int, j: Int) = elems(i)(j)

}

object Matrix
{
	def apply[T]( es: Vector[Vector[T]]) = {
		val colSizes = es map ( (v: Vector[T]) => v.size)
		if ( colSizes.exists(x => x != es(0).size))
      throw  new IllegalArgumentException("all rows should have the same size")
		else new Matrix(es)
	}
}
		
				
