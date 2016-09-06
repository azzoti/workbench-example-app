/*
 *  Operations specific to vectors
 */

package see.operations

import scala.collection.mutable.ListBuffer
import see.Binary
import see.Illegal
import see.ParamError
import see.Scope
import see.Unary
import see.Unresolved
import see.nodes.Node
import see.values._


// An operation that reduces a vector to a single scalar
// Works only on numbers for brevity. Anything else is esoteric anyway.
private[see] class ReduceOp(opc: String)( val op: (Number, Number) => Number)
extends Unary(opc)
{
	
	private def check(v: Val) = {
		val r = v.coerce
		try{
			r.asInstanceOf[Number]
		} catch {
			case _: Throwable => throw ParamError("Unsupported reduce operand: " + r)
		}
	}

	override def apply(s: Scope, v: Val): Val = v match {

		// requires vector with at least two elements
		case vx @ Vector(_ :: _) => {
				val start = check(apply(s, vx.values.head))
				(start /: vx.values.tail)( (accu, xi) => {
						op(accu, check(apply(s, xi)) )
					})
			}

		case _ => v
	}
}

// Reducing functions:

private[see] object Max extends ReduceOp("max")(
	(m, xi) =>  if (xi.cmp(m) > 0) xi else m )

private[see] object Min extends ReduceOp("min")(
	(m, xi) =>  if (xi.cmp(m) < 0) xi else m )

private[see] object Sum extends ReduceOp("sum")(_ + _)

private[see] object Prod extends ReduceOp("prod")(_ * _)

private[see] object Mean extends ReduceOp("mean")(_ + _) {
	override def apply(s: Scope, v: Val) = {
		super.apply(s, v) match {
			case n: Number => n / Lint(v.size)
			case e => e // assume error, should never show up
		}
	}
}

// folds vector elements from left, using any available binary operator
private[see] object Fold extends Unary("fold") {

	// needs 3 params: init, operation, vector
	override def apply(s: Scope, args: Val): Val = args match {
		case Vector(Seq(start, op, v)) => {
				var accu = start.coerce
				val foldOp = op.coerce
				val src = v // let operator decide whether to drill down further

				def fold(folder: Callable, vs: Seq[Val]) = {
					for (vn <- vs) accu = folder.call(s, Vector(List(accu, vn)))
					accu
				}

				(foldOp, src) match {
					case (folder: Callable, sc: Scalar) => {
							val args = new Vector(List(accu, sc))
							folder.call(s, args)
						}

					case (folder: Callable, Vector(vs)) =>
						fold(folder, vs)

					case _ => throw new ParamError(
							"Cannot fold " + src.getClass  +
							" with " + foldOp.getClass + ".")
				}
			}
		case _ => throw new ParamError(
				opCode + " requires (init, operation, value)." )
	}
}

// List generator:

// Generates vector with constant length
// The vector will never contain Anonyms,
// since that might cause strange side effects.
private[see] object Rep extends Unary("rep") {

	override def apply(s: Scope, args: Val): Val = args match {
		// needs 2 params:
		case Vector(Seq(ln, vn)) => {
				val l = ln.coerce.toLong.toInt
				if (l < 0)	throw new ParamError(
					opCode + " invalid length: " + ln)
				Vector(List.fill(l)(vn.coerce))
			}
		case _ => throw new ParamError(opCode + " requires (length, value)." )
	}
	

}

// Pads existing vector to given length
// Resulting vector may be shorter or longer
// The vector will never contain Anonyms,
// since that might cause strange side effects.
private[see] object Pad extends Unary("pad") {
	override def apply(s: Scope, args: Val): Val = args match {
		// needs 3 params:
		case Vector(Seq(src, ln, vn)) => {
				val len = ln.coerce.toLong.toInt
				if (len < 0)
					throw new ParamError(opCode + " invalid length: " + len)

				val buffer = ListBuffer[Val]()
				src match {
					case Vector(vals) => buffer ++= vals
					case x => buffer += x
				}
				if (buffer.size > len)
					buffer.remove(len, buffer.size - len)
				else
					// apply here, so vector won't get stuffed with closures
				for (c <- buffer.size until len) { buffer += vn.coerce }
				Vector(buffer)
			}
		case _ => throw new ParamError(
				opCode + " requires (source, length, value)." )
	}
}


/** Basically converts rows into columns and vice versa.
 */
private[see] object Zip extends Unary("zip") {
	override def apply(s: Scope, v: Val): Val = v match {
		// needs at least 2 params:
		case Vector(vs) => {
				if (vs.size >= 2) {
					if (!vs.forall(_.isInstanceOf[Vector]))
						throw new ParamError(opCode + " requires (vector1, vector2)." )
					else Vector(merge(vs.asInstanceOf[List[Vector]]))
				}
				else if (vs.size == 1) // assume arument is list of vects
					apply(s, vs.head)
				else
					throw new ParamError(opCode + " requires (vector1, vector2)." )
			}
		case _ => throw new ParamError(opCode + " requires (vector1, vector2)." )
	}

	private	def merge(vects: Seq[Vector]): Seq[Val] = {
		var sz = (vects.head.size /: vects.tail)(_ min _.size)
		val ll = for (i <- 0 until sz ) yield
			Vector(for (vect <- vects) yield vect.values(i))
		ll.toSeq
	}
}


/** Sorts a vector of comparable items in ascending order.
 * All other values remain unchanged.
 */
private[see] object Sort extends Unary("sort") {
	override def apply(s: Scope, v: Val): Val = v match {
		case Vector(vs) => {
				if (!vs.forall(_.isInstanceOf[Comparable]))
					return v

				val nv = vs.asInstanceOf[List[Comparable]].sortWith((a,b) => a < b)
				new Vector(nv)
			}
		case _ => v
	}
}

/** Removes exact duplicates from a vector.
 */
private[see] object Distinct extends Unary("distinct") {
	override def apply(s: Scope, v: Val): Val = v match {
		case Vector(vs) => new Vector(vs.distinct)
		case _ => v
	}
}

/** Removes liberal duplicates from a vector.
 */
private[see] object Unique extends Unary("unique") {
	def unify(vs: Seq[Val]) = {
		val r = ListBuffer[Val]()
		vs.foreach {v =>
			if (!r.exists(v.isEqualTo))
				r += v
		}
		Vector(r)
	}
	override def apply(s: Scope, v: Val): Val = v match {
		case Vector(vs) => unify(vs)
		case _ => v
	}
}

private[see] object At extends Binary("@") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (Str(x), r: Number) => {
					val index = r.toLong
					if (index >= x.size || index < 0 && -index > x.size)
						throw new Unresolved("Index:" + r.toLong)
					else if (index < 0)
						Str(x(x.size + index.toInt).toString)
					else Str(x(index.toInt).toString)
				}

			case (c: Container, _) => c.at(rhs)
			case (_, Vector(k)) => {
					if (k.isEmpty) lhs
					else {
						val r0 = apply(lhs, k.head)
						apply(r0, Vector(k.tail))
					}
				}
			case (_, r: Number) => {
					if (r.toLong == 0 || r.toLong == -1) lhs
					else throw new Illegal("Illegal dereference")
				}
			case _ => throw new Illegal("Illegal dereference")
		}

	override def isDefinedFor(s: Scope, lhs: Node, rhs: Node) =
		(lhs isDefinedIn s) && (rhs isDefinedIn s) && {
			val index = (rhs evalIn s).coerce
			val c = lhs evalIn s
			try {
				apply(c, index)
				true
			} catch { case _: Throwable => false }
		}
}

private[see] object Slice extends Binary("@@") {

	// Always produces a vector. If totally out of bounds, the result is empty.
	def vslice[T](v: Seq[T], s: Number, e: Number): Seq[T] = {
		val start = s.toLong.toInt
		val end = e.toLong.toInt
		var si = if (start < 0) v.size + start + 1 else start
		var ei = if (end < 0) v.size + end + 1 else end
		//println("si="+si + " ei=" + ei)
		if (ei > si){
			if (ei > v.size) ei = v.size
			if (si >= v.size || si < 0) Nil
			else v.slice(si, ei)
		} else {
			if (si > v.size) si = v.size
			if (ei >= v.size || ei < 0) Nil
			v.slice(ei, si).reverse
		}
	}

	private def mkslice[T](v: Seq[T], l: Seq[_]): Seq[T] = l match {
		case Seq(s: Number, e: Number) => vslice(v, s, e)
		case _ 	=>	throw new Illegal("Illegal slice range")
	}
	
	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (Str(x), Vector(l)) => Str(mkslice(x, l).toString)
			case (Vector(x), Vector(l)) => Vector(mkslice(x, l))
			case (_, Vector(k) )  => {
					// assume higher dimension slice
					//println("e: lhs = " + lhs)
					//				println("k="+k)
					if (k.isEmpty)
						lhs
					else {
						if (!k.forall(_.isInstanceOf[Vector]))
							throw new Illegal("Illegal dereference")
						val r0: Vector = apply(lhs, k.head).asInstanceOf[Vector]
						val r1 = for (v <- r0.values) yield apply(v, Vector(k.tail))
						Vector(r1.toList)
					}
				}
			case _ => throw new Illegal("Illegal dereference")
		}

	// default isDefined is fine...

}
