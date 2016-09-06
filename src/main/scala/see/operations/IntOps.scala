/*
 *  Operations that require integral operands.
 */

package see.operations

import see.BinIntOp
import see.Scope
import see.Unary
import see.values.Bool
import see.values.IntLike
import see.values.Val

private[see] object UnaryInv extends Unary("~") {
	override def apply(s: Scope, v: Val): Val = v match {
		case Bool(x) => Bool(!x)
		case x: IntLike => x.~
		case _ => super.apply(s, v)
	}
}

private[see] object BitAnd extends BinIntOp("&") (_ & _)
private[see] object BitOr extends BinIntOp("|") (_ | _)
private[see] object BitXor extends BinIntOp("^") (_ ^ _)
private[see] object BitRshift extends BinIntOp(">>") (_ >> _)
private[see] object BitLshift extends BinIntOp("<<") (_ << _)
