/*
 *  Boolean Operations (operands are forced to bool).
 */

package see.operations

import see.Binary
import see.Scope
import see.Unary
import see.nodes.Node
import see.values._

private[see] object UnaryNot extends Unary("!") {
	override def apply(s: Scope, v: Val): Val = v match {
		case Bool(x) => Bool(!x)
		case n: Number => Bool(!n.toBool)
		case _ => super.apply(s, v)
	}
}

private[see] object BoolAnd extends Binary("&&") {
	override def needsRhs(lhs: Val) = lhs.toBool
	override def apply(lhs: Val, rhs: Val): Val = Bool(lhs.toBool && rhs.toBool)
	override def isDefinedFor(s: Scope, lhs: Node, rhs: Node) =
		(lhs isDefinedIn s) && ((rhs isDefinedIn s) || !lhs.evalIn(s).coerce.toBool)
}
private[see] object BoolOr extends Binary("||") {
	override def needsRhs(lhs: Val) = !lhs.toBool
	override def apply(lhs: Val, rhs: Val): Val = Bool(lhs.toBool || rhs.toBool)
	override def isDefinedFor(s: Scope, lhs: Node, rhs: Node) =
		(lhs isDefinedIn s)  && ((rhs isDefinedIn s) || lhs.evalIn(s).coerce.toBool)
}
private[see] object BoolXor extends Binary("^^") {
	override def apply(lhs: Val, rhs: Val): Val = Bool(lhs.toBool ^ rhs.toBool)
}
