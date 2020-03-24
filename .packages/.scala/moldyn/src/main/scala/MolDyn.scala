package MolDyn

import scala.math._
import Physical._
import Physical.PHType._
import Physical.Field._
import Physical.SummableScalable._




//dont understand why just Mass makes a compile problem
object MDType{
	class Particle(val label: Int, val mass: PHType.Mass, var current: Position, var previous: Position){
		override def toString = label.toString + ": " + mass.value.toString + ", " + current.xval.value.toString + ", "+ current.yval.value.toString + ", "+ current.zval.value.toString

		def --> (rnew: Position): Particle = { //this cheats, mutates and returns itself
			previous = current
			current = rnew
			this
		}
	}
	object Particle{
		def apply(l: Int, m: PHType.Mass, c: Position, p: Position) = new Particle(l,m,c,p)
		def apply(l: Int, m: PHType.Mass, c: Position) = new Particle(l,m,c,c)
		def apply(l: Int, m: Double, c: (Double, Double, Double)): Particle = Particle(l, PHType.Mass(m), Position(c._1, c._2, c._3))
	}


	implicit object ForceFieldAdder extends SummableScalable[ForceVec]{
		def plus(f: ForceVec, g: ForceVec) = f + g
		def minus(f: ForceVec, g: ForceVec) = f - g
		def scale(f: ForceVec, s: Double) = f*s
		def identity = ForceVec(0., 0, 0.)
	}
	
	implicit object ForceField extends Field[Particle, ForceVec]{
		def plus(f: Particle => ForceVec, g: Particle => ForceVec) = 
			(p: Particle) => f(p) + g(p)
		
		def minus(f: Particle => ForceVec, g: Particle => ForceVec) = 
			(p: Particle) => f(p) - g(p)

		def scale(f: Particle => ForceVec, s: Double) =
			(p: Particle) => f(p)*s
	
		def identity = (p: Particle) => ForceVec(0., 0., 0.) 
			
	}
		
	
	type ForceFromParticle = Particle => ForceVec
	implicit object InterParticleForce extends Field[Particle, ForceFromParticle]{
		def plus(f: Particle => ForceFromParticle, g: Particle => ForceFromParticle) = 
			(p1: Particle) => { (p2: Particle) => f(p1)(p2) + g(p1)(p2) }
		
		def minus(f: Particle => ForceFromParticle, g: Particle => ForceFromParticle) = 
			(p1: Particle) => { (p2: Particle) => f(p1)(p2) - g(p1)(p2) }

		def scale(f: Particle => ForceFromParticle, s: Double) =
			(p1: Particle) => { (p2: Particle) => f(p1)(p2)*s}
	
		def identity = (p1: Particle) => { (p2: Particle) => ForceVec(0., 0., 0.)}
			
	}

	type EnergyWithParticle = Particle => Energy
	implicit object InterParticleEnergy extends Field[Particle, EnergyWithParticle]{
		def plus(f: Particle => EnergyWithParticle, g: Particle => EnergyWithParticle) = 
			(p1: Particle) => { (p2: Particle) => f(p1)(p2) + g(p1)(p2) }
		
		def minus(f: Particle => EnergyWithParticle, g: Particle => EnergyWithParticle) = 
			(p1: Particle) => { (p2: Particle) => f(p1)(p2) - g(p1)(p2) }

		def scale(f: Particle => EnergyWithParticle, s: Double) =
			(p1: Particle) => { (p2: Particle) => f(p1)(p2)*s}
	
		def identity = 
			(p1: Particle) => { (p2: Particle) => Energy(0.)}
	}

}
			
		
	

object forceFields{
	import MDType._
	type ForceFromParticle = Particle => ForceVec
	def LennardJones(e: Energy, s: Length): Particle => ForceFromParticle = (p: Particle) => {  
		(q: Particle) => {
			val r = q.current - p.current
			ForceVec( 48.*(e/r.amp)*(pow( s/r.amp, 12.) - 0.5*pow( s/r.amp, 6.)), r.dir)
		}
	}
}


object updateSchemes{
	import MDType._
	def verletUpdated(dt: PHType.Time,f: ForceVec)(a: Particle): Particle = 
		a --> (2.*:a.current - a.previous + ((f/a.mass)*dt)*dt)
}


//type InterParticleForce = Particle => Particle => ForceVec

//non mutable MD, doesnt (seem to) mutate anything.
import MDType._
object updatedMolDynSys{

	def apply(ffs: Seq[Particle => Particle => ForceVec], moved: (PHType.Time,  ForceVec) => Particle => Particle): PHType.Time => Seq[Particle] => Seq[Particle] = 
		apply( Field.sum(ffs), moved)

	def apply(fofi: Particle => Particle => ForceVec, moved: (PHType.Time,  ForceVec) => Particle => Particle): PHType.Time => Seq[Particle] => Seq[Particle] = {

		def forceOnParticle(atoms: Seq[Particle])(a: Particle): ForceVec = 
			SummableScalable.sum(for (b <- atoms if b != a) yield fofi(b)(a))
		
		(dt: PHType.Time) => (atoms: Seq[Particle]) => 
			for (a <- atoms) yield moved(dt, forceOnParticle(atoms)(a))(a)
	}
}


//MolDyn is defined for a bunch of forceFields and an update scheme, it does not maintain a state
//we define an MD with state
class MolDyn( ffs: Seq[Particle => Particle => ForceVec], val moved: (PHType.Time, ForceVec) => Particle => Particle, btoms: Seq[Particle]){
	val dt = PHType.Time(1.e-6)
	var time = PHType.Time(0.)
	var atoms = btoms
	
	def updatedAll = updatedMolDynSys(ffs, moved)(dt)

	def run( trun: PHType.Time) {
		while( time < trun){
		//	val updfs = for (a <- atoms) yield updatedParticle(dt, forceOnParticle(atoms)(a))
			//atoms = updatedAll(atoms, updfs)
			atoms = updatedAll(atoms) 
			time += dt
		}
	}
}
	
		

		
		
	



