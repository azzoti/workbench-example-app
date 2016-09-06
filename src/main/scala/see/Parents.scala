/*
 *  Parents.scala
 *  Defines different parents for a scope.
 */

package see

import see.Scope.CONSTNAME
import see.values.NullVal
import see.values.Val

private object Parent {

	def apply(r: Resolver) = r match {
		case null => NoParent
		case s: Scope => new OuterScope(s)
		case rs: Resolver => new WithResolver(rs)
	}
}

private trait Parent {
	val resolver: Resolver
	val stable: Boolean
	val stableCheck = false
	def contains(name: String) : Boolean
	def set(name: String, v: Val): Boolean
	def getVar(name: String): Option[Val]
	def getConst(name: String): Option[Val]
	def shallow: String
	def inner(s: Scope) = new Scope(s.parser, new OuterScope(s))
	def copy(cc: CopyContext): Parent =
		throw new RuntimeException("Unexpected copy from " + getClass)

}

private object NoParent extends Parent {

	override val resolver: Resolver = null
	// Because we might attach a parent later on
	// Stability must not change by that
	override val stable = false

	override def contains(name: String) = false
	override def set(name: String, v: Val) = false
	override def getVar(name: String) = None
	override def getConst(name: String) = None
	override def copy(cc: CopyContext) = NoParent
	override def shallow = ""
	override def toString = "No Parent"
}

private class OuterScope(val outer: Scope) extends Parent {

	override val resolver: Resolver = outer
	override val stable = false

	override def contains(name: String) = outer contains name
	override def set(name: String, v: Val) = {
		if (outer contains name) {outer.set(name, v); true}
		else false
	}
	override def getVar(name: String) = outer getVar name
	override def getConst(name: String) = outer getConst name
	override def copy(cc: CopyContext) = new OuterScope(outer.copy(cc))

	override def shallow = " ---> " + outer.shallow
	override def toString = "Parent Scope: " + outer
}

private class CowScope(outer: Scope) extends OuterScope(outer) {

	override def set(name: String, v: Val) = false
	override def copy(cc: CopyContext) = new CowScope(outer.copy(cc))

	override def toString = "Parent Scope (cow): " + outer
}

private class WithResolver(r: Resolver) extends Parent {

	override val resolver: Resolver = r
	override val stable = false

	override def contains(name: String) = r contains name
	override def set(name: String, v: Val) = {
		if (r contains name) {r.set(name, v); true}
		else false
	}
	override def getVar(name: String) = {
		val v= r get name
		if (v == null || v == NullVal) None
		else Some(Val(v))
	}
	override def getConst(name: String) = None // Resolver has no const
	override def copy(cc: CopyContext) = new WithResolver(r)

	override def shallow = " ---> Resolver"
	override def toString = "Parent Resolver: " + r
}

private class StableParent(outer: Scope) extends Parent {

	override val resolver: Resolver = outer
	override val stable = true // By definition

	override def contains(name: String) = outer contains name
	override def set(name: String, v: Val) = {
		if (outer contains name) throw new Illegal("Cannot export " + name)
		else false
	}
	override def getVar(name: String) =
		if (CONSTNAME.matches(name)) outer getConst name
	else None

	override def getConst(name: String) =
		if (CONSTNAME.matches(name)) outer getConst name
	else None
	override def copy(cc: CopyContext) = new StableParent(outer.copy(cc))

	override def shallow = " ---> " + outer.shallow
	override def toString = "Parent Scope: " + outer

	override def inner(s: Scope) = new Scope(s.parser, new StableParent(s))
}

private object StableScope extends Parent {

	override val resolver: Resolver = null
	override val stable = true // By definition

	override def contains(name: String) = false
	override def set(name: String, v: Val) = false
	override def getVar(name: String) = None
	override def getConst(name: String) = None
	override def shallow = ""
	override def toString = "No Parent(stable)"
	override def inner(s: Scope) = new Scope(s.parser, new StableParent(s))
	override def copy(cc: CopyContext) = StableScope
}


private class StableCheck(outer: Scope) extends StableParent(outer) {
	override val stableCheck = true
	override def inner(s: Scope) = new Scope(s.parser, new StableCheck(s));
	// Ony used during simplification. Should never show up afterwards
}

