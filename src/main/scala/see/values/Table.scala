/*
 *  
 */

package see.values

import see.Illegal
import see.ParamError
import see.ConstParser
import see.EvalError
import see.SeeException
import see.operations.Minus
import see.operations.Plus
import see.operations.Times
import see.StableScope
import see.operations.Div
import see.Scope
import Number._

private[see] object Table{

	// comes to void as near as we may get
	object Empty extends Table(List()) {
		override def toString = "EmptyTable"

		override def toBool: Boolean = false
		override def toLong: Long = 0
		override def size: Int = 0
		override def isFlat = true
		override def map(mapf: Val => Val) = this
	}

	def apply(init: Val): Table = new Table( init match {
			case a: Assoc if (a.key.isInstanceOf[Number]) =>
				List(new TP(a.key.asInstanceOf[Number], a.value))
			case n: Number => List(new TP(n, n))

			case Vector(vs) => {
					if (vs.isEmpty) return Empty // ensure empty tables compare equal.

					val seq = for (v <- vs) yield { v match {
							case a: Assoc if (a.key.isInstanceOf[Number]) =>
								new TP(a.key.asInstanceOf[Number], a.value)
							case n: Number => new TP(n, n)
							case _ => throw new ParamError("Unsuitable table key:" + init)
						}
					}
					seq.toList
				}
			case _ => throw new ParamError("Unsuitable table key:" + init)
		}).verify

	// Scope required by interpolation
	private val ATSCOPE = Scope.clean
}

private[see] class TP(val x: Number, val y: Val){
	override def toString = x.toStr + " -> " + y.toStr
}

private[see] case class Table(private val points: List[TP])
extends Container {


	override def selType = 'Table
	override def isType(typeId: Symbol) =
		(typeId == 'Table) || super.isType(typeId)

	override def size = points.size
	override def isEmpty = points.isEmpty

	private def verify = {
		if (!isEmpty){
			var p0 = points.head
			for (p <- points.tail){
				if (p.x < p0.x) throw new EvalError("Corrupt table order")
				p0 = p
			}
		}
		this
	}

	override def at(index: Val) = interpolate(index, Table.ATSCOPE)
	override def call(s: Scope, args: Val) = interpolate(args, s)

	override def foreach(v: Val => Unit) = points.foreach(_.y)
	// No set operations:
	def vals = Nil.toIterator
	def assocs = Nil.toIterator
	def contains(rhs: Val) = rhs.isInstanceOf[Number]
	def containsWeak(rhs: Val) = rhs.isInstanceOf[Number]
	def containsStrong(rhs: Val) = rhs.isInstanceOf[Number]
	def union(rhs: Val) = throw new Illegal("@|")
	def intersect(rhs: Val)= throw new Illegal("@&")
	def diff(rhs: Val) = throw new Illegal("@^")

	override def map(mapf: Val => Val) = Table(
		for (v <- points) yield new TP(v.x, mapf(v.y).coerce)
	)
	
	override def addLeft(v: Val) = new Table( v match {
			case Table(opts) => opts ::: points
			case Assoc(l: Number, r: Val) => new TP(l, r) :: points
			case n: Number => new TP(n, n) :: points
			case _ => throw new ParamError("Unsuitable table key:" + v)
		}).verify

	override def addRight(v: Val) = new Table( v match {
			case Table(opts) => points ::: opts
			case Assoc(l: Number, r: Val) => points :+ new TP(l, r)
			case n: Number => points :+ new TP(n, n)
			case _ => throw new ParamError("Unsuitable table key:" + v)
		}).verify

	override def forall(pred: Val => Boolean) =
		points.forall{vv:TP => pred(vv.y)}
	override def deepCmp(rhs: Val): Boolean = rhs match{
		case Table(rs) => {
				for ((pi,pj) <- points.zip(rs))
					if (!pi.x.isEqualTo(pj.x) || !pi.y.isEqualTo(pj.y))
						return false
				true
			}
		case _ => false
	}

    override def toString = points.mkString("Table(", ", ", ")")
	override def toStr: String = points.mkString("table(", ", ", ")")

	override def toJava = this

	private def interpolate(pos: Val, sc: Scope): Val = {
		if (isEmpty) return Lint.Zero
		var p0 = points.head
		val (x, args) = pos match {
			case n: Number => (n, Vector.Empty)
			case Vector(h :: t) if h.isInstanceOf[Number] =>
				(h.asInstanceOf[Number], Vector(t))
			case Vector(h) if h.isInstanceOf[Number] =>
				(h.asInstanceOf[Number], Vector.Empty)
			case err => throw new ParamError("Unsuitable table key: " + err )
		}
		var y0 = ipPoint(p0.y, args, sc)

		// find applicable table partition:
		if(points.size > 1 && x > p0.x) try {
			for (p1 <- points.tail){
				val diff = x - p0.x
				val dx = p1.x - p0.x
				val y1 = ipPoint(p1.y, args, sc)
				// never interpolate beyond last point
				if (diff >= Lint.Zero && diff < dx) {
					val r = y0 match {
						case f: Functional => f.call(sc, x)
						case t: Table => t.call(sc, x)
							// interpolate for anything else
						case y => {
								val dy = Minus(y1, y0)
								// By doing division down here,
								// we will get the simplest result type possible
								Plus(y0, Times(diff, Div(dy, dx)))
							}
					}
					//println("Interpolated: " + r)
					return r
				}
				p0 = p1
				y0 = y1
			}
		} catch {
			case ex: SeeException => // stay with y0
		}
		//println("Result is: " + y0)
		y0 match {
			// if we had a callable, use it instead of interpolation
			case c: Callable => c.call(sc, x)
			case y => y
		}
	}

	private def ipPoint(pos: Val, args: Vector, sc: Scope): Val =
	{
		pos match {
			case c: Callable => 
				if (args.isEmpty) c
				else c.call(sc, args).coerce
			case x => x.coerce
		}
	}
}

