/*
 *  Setlike operations work upon vectors and maps.
 */

package see.operations

import see.Binary
import see.Relator
import see.values._

private[see] object WeakContainment extends Relator("@?") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (c: Container, _) => Bool(c.containsWeak(rhs))
			case (_, c: Container) => Bool.False
			case _ => Bool(lhs.isEqualTo(rhs))
		}

}

private[see] object StrongContainment extends Relator("@??") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (c: Container, _) => Bool(c.containsStrong(rhs))
			case (_, c: Container) => Bool.False
			case _ => Bool(lhs == rhs)
		}

}


private[see] object SetUnion extends Binary("@|") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {

			case (c: Container, _) => c.union(rhs)
			case (_, c: Container) => c.union(lhs)
			case _ => new ValMap(Map(lhs->lhs, rhs->rhs))
		}
}

private[see] object SetIntersection extends Binary("@&") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {

			case (c: Container, _) => c.intersect(rhs)
			case (_, c: Container) => c.intersect(lhs)
			case _ => if (lhs.isEqualTo(rhs)) new ValMap(Map(lhs->lhs))
				else ValMap.Empty
		}
}

private[see] object SetDifference extends Binary("@^") {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (c: Container, _) => c.diff(rhs)
			case (_, c: Container) => if (c.contains(lhs)) ValMap.Empty
				else new ValMap(Map(lhs->lhs))
			case _ => if (lhs.isEqualTo(rhs)) ValMap.Empty
				else new ValMap(Map(lhs->lhs))
		}
}


