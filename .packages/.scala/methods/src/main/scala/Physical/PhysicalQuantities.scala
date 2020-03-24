package Physical


/* lets try to learn the type class pattern by defining physical quantities.
*/
//everything be a function
object PHQuantity{

	trait Quantity{
		def value: Double
		def +(that: Quantity): Quantity =  arith.add(this, that)
		def -(that: Quantity): Quantity =  arith.sub(this, that)
		def *(that: Quantity): Quantity = arith.mul(this, that)
		//def /(that: Quantity): Quantity = arith.div(this, that) 
		def *(s: Double): Quantity = arith.mul(this, s)
		def /(s: Double): Quantity = arith.div(this, s)
		def *:(s: Double): Quantity = this*s

		def compare(that: Quantity): Int = arith.compare(this, that)
			
		def ==(that: Quantity): Boolean = (this compare that) == 0
		def >(that: Quantity): Boolean = (this compare that) == 1
		def <(that: Quantity): Boolean = (this compare that) == -1
		def >=(that: Quantity): Boolean = (this == that) || (this > that)
		def <=(that: Quantity) = (this == that) || (this < that)
		def !=(that: Quantity) =  !(this == that)
	}

	/*
	class PHVector(val value: (Double, Double, Double)){
		def + (that: PHVector) = new PHVector(this.value._1 + that.value._1, this.value._2 + that.value._2, this.value._3 + that.value._3) 

		def - (that: PHVector) = new PHVector(this.value._1 - that.value._1, this.value._2 - that.value._2, this.value._3 - that.value._3) 

		def * (s: Double) = new PHVector(this.value._1 * s, this.value._2 * s, this.value._3 * s) 

		def *: (s: Double) = this*s
		
		def / (s: Double) = new PHVector(this.value._1 / s, this.value._2 / s, this.value._3 / s) 
		
	}
	*/

	case class Length(value: Double) extends Quantity
	case class Area(value: Double) extends Quantity
	case class Volume(value: Double) extends Quantity
	case class Mass(value: Double) extends Quantity
	case class Time(value: Double) extends Quantity
	case class Temperature(value: Double) extends Quantity
	case class Speed(value: Double) extends Quantity
	case class Accel(value: Double) extends Quantity
	case class Force(value: Double) extends Quantity
	case class Energy(value: Double) extends Quantity

	//case class Position(value: PHVector) extends Quantity
	//case class Velocity(value: PHVector) extends Quantity
	//case class AccVec(value: PHVector) extends Quantity
	//case class ForceVec(value: PHVector) extends Quantity

	object arith{
		
		def add(x: Quantity, y: Quantity): Quantity = (x, y) match{
			case (Length(l1), Length(l2)) => Length(l1 + l2)
			case (Mass(m1), Mass(m2)) => Mass(m1 + m2)
			case (Time(t1), Time(t2)) => Time(t1 + t2)
			case (Temperature(t1), Temperature(t2)) => Temperature(t1 + t2)
			case (Speed(x), Speed(y)) => Speed(x + y)
			case (Accel(x), Accel(y)) => Accel(x + y)
			case (Force(x), Force(y)) => Force(x+y)
			case (Energy(x), Energy(y)) => Energy(x+y)
			case _ => throw new Error("Incompatible Add")
		}

		def sub(x: Quantity, y: Quantity): Quantity = (x, y) match{
			case (Length(l1), Length(l2)) => Length(l1 - l2)
			case (Mass(m1), Mass(m2)) => Mass(m1 - m2)
			case (Time(t1), Time(t2)) => Time(t1 - t2)
			case (Temperature(t1), Temperature(t2)) => Temperature(t1 - t2)
			case (Speed(x), Speed(y)) => Speed(x - y)
			case (Accel(x), Accel(y)) => Accel(x - y)
			case (Force(x), Force(y)) => Force(x-y)
			case (Energy(x), Energy(y)) => Energy(x-y)
			case _ => throw new Error("Incompatible Add")
		}

		def mul(x: Quantity, s: Double): Quantity = x match{
			case Length(l1) => Length(s*l1)
			case Mass(m1) => Mass(s*m1)
			case Time(t1) => Time(s*t1)
			case Temperature(t1) => Temperature(s*t1)
			case Speed(x) => Speed(s*x)
			case Accel(x) => Accel(s*x)
			case Force(x) => Force(s*x)
			case Energy(x) => Energy(s*x)
			case _ => throw new Error(" multiply with unknown Quantity")
		}
			
		def div(x: Quantity, s: Double): Quantity = x match{
			case Length(l1) => Length(s/l1)
			case Mass(m1) => Mass(s/m1)
			case Time(t1) => Time(s/t1)
			case Temperature(t1) => Temperature(s/t1)
			case Speed(x) => Speed(s/x)
			case Accel(x) => Accel(s/x)
			case Force(x) => Force(s/x)
			case Energy(x) => Energy(s/x)
			case _ => throw new Error(" multiply with unknown Quantity")
		}
			

		def mul(x: Quantity, y: Quantity): Quantity = (x, y) match{
			case (Length(l1), Length(l2)) => Area(l1*l2)
			case (Length(l), Area(a)) => Volume(l*a)
			case (Area(a), Length(l)) => Volume(l*a)
			case (Speed(s), Time(t)) => Length(s*t)
			case (Accel(a), Time(t)) => Speed(a*t)
			case (Force(f), Length(l)) => Energy(f*l)
			case _ => throw new Error("unknown multiplication")
		}
		

		def compare(x: Quantity, y: Quantity): Int = (x, y) match{
			case (Length(l1), Length(l2)) => l1 compare l2
			case (Mass(m1), Mass(m2)) => m1 compare m2
			case (Time(t1), Time(t2)) => t1 compare t2
			case (Temperature(t1), Temperature(t2)) => t1 compare t2
			case (Speed(x), Speed(y)) => x compare y
			case (Accel(x), Accel(y)) => x compare y
			case (Force(x), Force(y)) => x compare y
			case (Energy(x), Energy(y)) => x compare y
			case _ => throw new Error("Incompatible Comparison")
		}

	}
		
}
		
		
