/*
 *  
 */

package see.values

import see.CopyContext
import see.Scope
import see.Unary
import see.nodes.Node
import see.nodes.Fnode
import see.operations.Rnd

/** Generic closure, not a value type!
 * Any closure will keep track of its definition scope.
 */
private[see] abstract class Closure extends Val
{
	override def selType = 'Closure
	override def isType(typeId: Symbol) = (typeId == 'Closure) || super.isType(typeId)

	override def toJava: AnyRef = this
	override def size = 1

	override def toBool: Boolean = false

}


/** An anonymous function without parameters.
 * This is the result from evaluationg a block.
 */
private[see] case class Anonym(scope: Scope, node: Node )
extends Closure
{
	override def selType = 'Anonymous
	override def isType(typeId: Symbol) = (typeId == 'Anonymous) || super.isType(typeId)

    override def toString = "<<< Anonym within " + scope.shallow + ">>>"

	override def toJava: AnyRef = coerce.toJava


	override def coerce: Val = {
		// To prevent inner $ from escaping, if something like $ += k is used,
		// we ensure that it is set locally.
		// Note that this WON'T change the value, if the closure has
		// already been dereferenced
		scope.setResult(scope.getResult)

		//println ("Scope before apply "+ scope)
		// Note the final collapse to handle the case
		// where a closure again yields another one
		val v = (node evalIn scope).coerce
		//println ("Scope after apply " + scope)
		//println ("Returns " + v)
		v
	}
	override def copy(cc: CopyContext) = new Anonym(scope.copy(cc), node)


	override def dump = toString + "\ndefined as: " + node.dump

}

/** A closure requiring a parameter list.
 * The list may be empty, but it has to be supplied.
 * Such closures must be explicitly called.
 */
private[see] abstract class Functional(val scope: Scope)
extends Closure with Callable
{
	override def selType = 'Function
	override def isType(typeId: Symbol) = (typeId == 'Function) || super.isType(typeId)

    override def toString = "<<< Function " + name +
	" within " + scope.shallow + ">>>"

	def name: String

	// callerScope is actually discarded by all subclasses,
	// because they always use their recorded scope,
	// but included for consistency
	// def call(callerScope: Scope, args: Val): Val
	//
}


/** Result of an evaluated function definition.
 */
private[see] class UserFunc(s: Scope, val fdef: Fnode)
extends Functional(s)
{
	override def name = fdef.name
	override def isDefinedIn(s: Scope) = fdef.isDefinedIn(s)

	override def call(callerScope: Scope, args: Val)= fdef.call(scope, args)
	override def copy(cc: CopyContext) = new UserFunc(scope.copy(cc), fdef)

	override def dump = toString + "\nfdef: " + fdef.dump
	override def isStable = fdef.stable

}


/** Result of a function nesting operation ( x = f * g ).
 */
private[see] class NestedFunc(val outer: Functional, val inner: Functional)
extends Functional(outer.scope)
{
	override def name = outer.name + "(" + inner.name + "())"
	override def isDefinedIn(s: Scope) =
		outer.isDefinedIn(s) && inner.isDefinedIn(s)
 
	override def call(callerScope: Scope, args: Val) = {
		val t = inner.call(callerScope, args)
		outer.call(callerScope, t)
	}
	override def copy(cc: CopyContext) = new NestedFunc(
		outer.copy(cc).asInstanceOf[Functional],
		inner.copy(cc).asInstanceOf[Functional])

	override def dump = (toString
						 +	"\nouter: " + outer.dump
						 + "\ninner: " + inner.dump)
	override def isStable = outer.isStable && inner.isStable

}

/** Wrapper that allows to use a built-in function as value.
 * In general, the scope will be ignored in such a case anyway.
 */
private[see] class BuiltinFunc(s: Scope, val op: Unary)
extends Functional(s)
{
	override def name = op.opCode + "()"
	override def isDefinedIn(s: Scope) = true

	override def call(callerScope: Scope, args: Val) = op(scope, args)
	override def copy(cc: CopyContext) = new BuiltinFunc(scope.copy(cc), op)

	override def dump = toString + "\nbuiltin: " + op.opCode
	override def isStable = !(op eq Rnd)

}


