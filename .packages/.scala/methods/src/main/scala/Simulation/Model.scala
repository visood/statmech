package Simulation

//import scala.annotation.tailrec
import language.postfixOps
//import language.implicitConversions


//we need to compose time into the state
	

//so we want to have a generalized concept of time,
trait Timer[T]
{ //a clock of type T
  def stop(ts: T)(t: T): Boolean
  def startedAt: T
  def stoppedAt: T
  def decrement(t: T, dt: T): T
  def increment(t: T, dt: T): T
}

object Timers
{
  def integerTimer(t0: Int, t1: Int): Timer[Int] = new Timer[Int]{ 
    val startedAt = t0
    val stoppedAt = t1
    def stop(ts: Int)(t: Int) = (t >= ts)
    def decrement(t: Int, dt: Int) = t - dt
    def increment(t: Int, dt: Int) = t + dt
  }
}

object implicitTimers
{
  implicit val intime: Timer[Int] = Timers.integerTimer(0, -1)
}

	
object functor
{//may be we should a less pretentious name
	def apply[S, T](f: S => T): Function1[S, T] = {
		new Function1[S, T]{
			def apply(s: S): T = f(s)
		}
	}
}


trait DynamicalModel[T, S]
{ //T: time, S: State
  import scala.annotation.tailrec

  def apply(t: T, xs: S): (T, S)
  val updatedOnce: ((T, S)) => (T, S) = functor(apply _ tupled) // as a Function1 this can be composed
		
  @tailrec final def updatedTill(tstop: T)(state: (T, S))(implicit timer: Timer[T]): (T, S) = 
    if (timer.stop(tstop)(state._1)) state
    else updatedTill(tstop)(updatedOnce(state))

  def updatedFor(time: T)(s: S)(implicit timer: Timer[T]): S = 
    updatedTill(time)((timer.startedAt, s))._2

  def trajectoryFrom(xs: S)(implicit timer: Timer[T]): Stream[(T, S)] = {

    def loop(xt: (T, S)): Stream[(T, S)] = xt #:: loop( updatedOnce(xt) )

    loop((timer.startedAt, xs))
  }
}

object SimpleModelExamples
{
  import implicitTimers._
  def RandomWalk(implicit rg: util.Random) = new DynamicalModel[Int, Int]{

    def apply(t: Int, x: Int): (Int, Int) = (t+1, x + 2*rg.nextInt(2) - 1)

  }
}
		

  




