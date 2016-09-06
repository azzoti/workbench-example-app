/*
 * Miscellaneous operands.
 *
 */

package see.operations

import math._
import see.See
import see.Binary
import see.Relator
import see.Scope
import see.Unary
import see.nodes.Variable
import see.values._

// returns the See version string.
private[see] object Version extends Unary("version") {
	override def apply(s: Scope, v: Val) = Str(See.getVersion)
}

// returns the length of its operand in terms of elements.
private[see] object Length extends Unary("len") {
	override def apply(s: Scope, v: Val) = Lint(v.size)
}

// defined() checks if operand node is defined within current scope.
// but since it operates upon a node instead of a value,
// it cannot be written as ordinary operation.
// Therefore it is handled within Prefix.
//private[see] object Defined extends Unary('defined) {
//	override def apply(s: Scope, v: Val) =
//}



// Treats its evaluated operand as a variable name.
// This is quite special, use with care!
private[see] object Symbolize extends Unary("`") {

	override def apply(s: Scope, v: Val) = v.coerce match {
			case Vector(vs) => Vector(for (k <- vs) yield apply(s, k))
			// cannot call, no parameter list
			//case func: Callable => apply(func.call(s, ?))
			case x => {
					val sym = x.coerce.toStr
					Variable(sym) evalIn s
			}
		}
}


// Concatenates operands.
// Result type depends on operand. Can be either vector or string.
// Produces a flat return type, e.g. (1,2,3) ++ (4,5,6) will yield (1,2,3,4,5,6)
private[see] object Concat extends Binary("++") {
	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (Vector(x), Vector(y)) => Vector( x ++ y )
			case (c: Container, y) => c.addRight(y)
			case (x, c: Container) => c.addLeft(x)
			case (Str(x), Str(y)) => Str( x ++ y )
			case (Str(x), y) => Str( x ++ y.toStr )
			case (x, Str(y)) => Str( x.toStr ++ y )
			case _ => Vector(List(lhs, rhs))
		}
}

// Concatenates operands.
// Result will always be a vector.
// E.g. (1,2) +++ (3,4) yields ((1,2), (3,4))
private[see] object Vectorize extends Binary("+++") {
	override def apply(lhs: Val, rhs: Val): Val = Vector(List(lhs, rhs))
}
	

// Pattern matching operators:


// Tests for full match.
private[see] object BoolMatch extends Relator("~~") {
	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			// should be symmetrical
			case (x: Rexp, s: Scalar) => x ~ s
			case (s: Scalar , x: Rexp) => x ~ s
			case _ => super.apply(lhs, rhs)
		}
}

// Performs full match and returns matching result split into groups.
private[see] object Selection extends Binary("~~~") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			// should be symmetrical
			case (x: Rexp, s: Scalar) => x ~~ s
			case (s: Scalar , x: Rexp) => x ~~ s
			case _ => super.apply(lhs, rhs)
		}
}

// Searches for first match and returns matching position as Int.
private[see] object FindFirstPos extends Binary("~@") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			// should be symmetrical
			case (x: Rexp, s: Scalar) => x ~# s
			case (s: Scalar , x: Rexp) => x ~# s
			case _ => super.apply(lhs, rhs)
		}
}

// Searches for first match and returns matching result as vector.
private[see] object FindFirst extends Binary("~+") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			// should be symmetrical
			case (x: Rexp, s: Scalar) => x ~+ s
			case (s: Scalar , x: Rexp) => x ~+ s
			case _ => super.apply(lhs, rhs)
		}
}

// Searches for all matches and returns matching results as vector.
private[see] object FindAll extends Binary("~*") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			// should be symmetrical
			case (x: Rexp, s: Scalar) => x ~* s
			case (s: Scalar , x: Rexp) => x ~* s
			case _ => super.apply(lhs, rhs)
		}
}


// random generator, produces number between 0 and argument
private[see] object Rnd extends Unary("rnd") {
	private val gen = new scala.util.Random()
	override def apply(s: Scope, v: Val): Val = v match {
		case Lint(x) => Lint(abs(gen.nextLong()) % x)
		case Real(x) => Real(gen.nextDouble * x)
		case BigI(x) => BigI(BigInt(x.bitCount, gen) % x)
			// of questionable use anyway
		case BigR(x) => BigR(BigInt(x.toBigInt.bitCount, gen))
		case _ => super.apply(s, v)
	}
}

