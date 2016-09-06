/*
 *  Generic arithmetical operators.
 *  Note that some of them have a redefined meaning for other operand types.
 */

package see.operations

import see.BinIntOp
import see.BinNumOp
import see.Binary
import see.ParamError
import see.Scope
import see.Unary
import see.values._


private[see] object UnaryPlus extends Unary("+") {
	override def apply(s: Scope, v: Val): Val = v
}

private[see] object UnaryMinus extends Unary("-") {
	override def apply(s: Scope, v: Val): Val = v match {
		case n: Number => n.negate
		case _ => super.apply(s, v)
	}
}


private[see] object Gcd extends BinIntOp("gcd") (_ gcd _)

private[see] object Div extends BinNumOp("/") (_ / _)
private[see] object Mod extends BinNumOp("%") (_ % _)
private[see] object Exp extends BinNumOp("**") (_ ** _)

private[see] object Minus extends BinNumOp("-") (_ - _)
private[see] object Plus extends BinNumOp("+") (_ + _)
{
	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (l : Str, r: Val) => Str(l.toStr + r.toStr)
			case (l : Rexp, r: Val) => new Rexp(l.toStr + r.toStr)
			case _ => super.apply(lhs, rhs)
		}
}
private[see] object Times extends BinNumOp("*") (_ * _)
{

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			// repeated string concatenation:
			case (l : Str, r: Lint) => Str(l.toStr * r.toInt)
			case (l : Rexp, r: Lint) => new Rexp(l.toStr * r.toInt)
				// allow function combining:
			case (outer: Functional, inner: Functional) => 
				new NestedFunc(outer, inner)
			case _ => super.apply(lhs, rhs)
		}

}

private[see] object ScalProd extends Binary("*+") {
	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (l: Number, r: Number) => l * r
			case (l: Number, Vector(bs)) => bs(0) match {
					case r: Number => l * r
					case Vector(bbs) => Vector(for(bi <- bbs) yield apply(l, bi))
				}
			case (Vector(a), r: Number) =>
				Vector(for (ai <- a) yield apply(ai, r))

			case (as @ Vector(a), bs @ Vector(b)) => {
					if (as.isFlat) {
						if (bs.isFlat) {
							val bsize = b.size
							var sum: Number = Lint(0)
							for ((ai, i) <- a.zipWithIndex) {
								val bi = if (i >= bsize) Lint(1) else b(i)
								if (ai.isInstanceOf[Number] && bi.isInstanceOf[Number])
									sum += ai.asInstanceOf[Number] * bi.asInstanceOf[Number]
								else
									throw new ParamError("Scalar Product needs numeric vectors.")
							}
							sum
						}
						else Vector( for(bi <- b) yield apply(as, bi) )
					}
					else Vector( for(ai <- a) yield apply(ai, bs))
				}
			case _ => super.apply(lhs, rhs)
		}
}


//	private[see] object CrossProd extends Binary("*#") {
//		override def apply(lhs: Val, rhs: Val): Val =
//		(lhs, rhs) match {
//			case (l : Number, r: Number) => l * r
//			case _ => super.apply(lhs, rhs)
//		}
//	}
