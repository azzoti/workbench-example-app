/*
 * Relational operators (all producing bool results).
 */

package see.operations

import see.Relator
import see.values._

/** Test for exact equality (like in maps) */
private[see] object ExactEqual extends Relator("===") {
	override def apply(lhs: Val, rhs: Val) = Bool(lhs == rhs)
}

private[see] object ExactNe extends Relator("!==") {
	override def apply(lhs: Val, rhs: Val): Val =
		Bool(lhs != rhs)
}

/** Liberal equality (the default) */
private[see] object Equal extends Relator("==") {
	override def apply(lhs: Val, rhs: Val) = Bool(lhs.isEqualTo(rhs))
}

private[see] object Ne extends Relator("!=") {
	override def apply(lhs: Val, rhs: Val): Val =
		Bool(!lhs.isEqualTo(rhs))
}

private[see] object Less extends Relator("<"){
	override def apply(lhs: Val, rhs: Val): Val = {
		val l = lhs.coerce
		val r = rhs.coerce

		//println ("LT " + lhs + " < " +  rhs)
		(l, r) match {
			case (lc : Comparable, rc: Comparable) =>
				Bool( Comparable(lc,rc) < 0 )
			case (Vector(x), Vector(y)) => {
					if (x.size != y.size) Bool( x.size < y.size)
					else super.apply(l, r)
				}
			case _ => super.apply(l, r)
		}
	}
}

private[see] object Greater extends Relator(">") {
	override def apply(lhs: Val, rhs: Val): Val = {
		val l = lhs.coerce
		val r = rhs.coerce

		//println ("GT " + lhs + " > " +  rhs)
		(l, r) match {
			case (lc : Comparable, rc: Comparable) =>
				//println ("Cmp " + (l expand r) + " > " +  r)
				Bool( Comparable(lc, rc) > 0 )
			case (Vector(x), Vector(y)) => {
					if (x.size != y.size) Bool( x.size > y.size)
					else super.apply(l, r)
				}
			case _ => super.apply(l, r)
		}
	}
}

private[see] object Ge extends Relator(">=") {
	override def apply(lhs: Val, rhs: Val): Val =
		Bool(!Less.apply(lhs, rhs).toBool)
}
private[see] object Le extends Relator("<=") {
	override def apply(lhs: Val, rhs: Val): Val =
		Bool(!Greater.apply(lhs, rhs).toBool)
}
