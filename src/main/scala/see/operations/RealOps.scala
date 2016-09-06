/*
 *  Operations upon floating point numbers
 */

package see.operations

import math._
import see.ParamError
import see.RangeOverflow
import see.Scope
import see.Unary
import see.values._

// An operation upon numerical values only, recurses into vectors.
private[see] class RealOp(opc: String, val op: Double => Double)
extends Unary(opc) {

	private def propagate(t: Number, v: Double): Val = t match {
		case x: BigR => BigR(v)
		case x: BigI => BigR(v)
		case _ => Real(v)
	}

	protected def check(operand: Double) {}

	override def apply(s: Scope, v: Val): Val = v match {
		case x: Number => {
				val operand = x.toDouble
				if (operand.isInfinite || operand.isNaN)
					throw new RangeOverflow(
						"Operand overflow while calling " + opc + " on " + x)
				check(operand)
				val result = op(operand)
				if (result.isInfinite || result.isNaN)
					throw new RangeOverflow("Range overflow after calling " +
											opc + " on " + x + ": " + result)
				else
					propagate(x, result)

			}
		case _ => super.apply(s, v)
	}
}

private[see] object Sqrt extends RealOp("sqrt", sqrt) {
	// sqrt doesn't seem to catch domain errors correctly
	override def check(arg: Double) = {
		if (arg < 0) throw new ParamError("Illegal sqrt argument: " + arg)
	}
}

// Natural logarithm
private[see] object Log extends RealOp("log", log) {
	override def check(arg: Double) = {
		if (arg <= 0) throw new ParamError("Illegal log argument: " + arg)
	}
}
// Logarithm base 10
private[see] object Log10 extends RealOp("log10", log10) {
	override def check(arg: Double) = {
		if (arg <= 0) throw new ParamError("Illegal log10 argument: " + arg)
	}
}

// Power (same as ** operator, added for convenience only)
private[see] object Power extends Unary("pow") {
	override def apply(s: Scope, v: Val): Val = v match {
		// needs at exactly 2 params:
		case Vector(Seq(b,e)) => Exp.apply(b.coerce, e.coerce)
		//case Vector(Seq(x)) => apply(s, x)
		case _ => throw new ParamError(opCode + " requires (base, exp)." )
	}
}

// Trigonometry:
private[see] object Sin extends RealOp("sin", sin)
private[see] object Cos extends RealOp("cos", cos)
private[see] object Tan extends RealOp("tan", tan)

private[see] object ASin extends RealOp("asin", asin)
private[see] object ACos extends RealOp("acos", acos)
private[see] object ATan extends RealOp("atan", atan)

