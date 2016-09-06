/*
 * Scope.scala
 *
 */

package see

import scala.collection.JavaConversions._
import scala.collection.mutable.{Map => MMap}
import values._

private class CopyContext(val parser: Parser) {
	val scopes = MMap[Scope, Scope]()
}

private object Scope
{
	val RNAME = "$"

	// A constant consists of all-uppercase names.
	// At least one alpha character is required.
	//
	// Note that this pattern is just meant to check if a given name is a
	// constant. It will also match invalid names (e.g. 0A) and therefore
	// cannot be used to parse a name in the first case.
	//
	val CONSTNAME = new Regex("([0-9$#._]*[A-Z]+[0-9$#._]*)+")

	def apply(parent: OuterScope) = new Scope(parent.outer.parser, parent)
	
	// Creates an empty scope that will not accept new entries,
	// that are not explicitly injected from outside.
	def clean = new Scope(new ConstParser(), StableScope)
}

/** A scope within See.
 *
 * A scope contains a mapping of local names as well as an optional reference to
 * some outer scope that is queried for any name not found locally.
 *
 * A scope may be created for Constant evaluation only.
 * In such a case, creating and changing variables locally is still fine,
 * but any attempt to modify the outer scope will result in an evaluation error.
 */
private class Scope(val parser: Parser, var parent: Parent = NoParent)
extends See {
	import Scope._

	protected[see] val locals = MMap[String, Val]()

	final def parse(expression: String) = parser.parse(expression, this)
	
	final def eval(node: INode): IResult = try {
		exec(node)
	} catch {
		case ResultException(r) => setResult(r.coerce)
		case x: Exception => {
				setResult(NullVal) // undefine $ in case of error
				throw x; // and rethrow
			}
	}
	
	final def setLocal(name: String, v: Int) = locals.update(name, Lint(v))
	final def setLocal(name: String, v: Double) = locals.update(name, Real(v))
	final def setLocal(name: String, v: Boolean) = locals.update(name, Bool(v))

	// Mostly for debugging:
	override def toString = ( "Locals: " + locals
		//	+ locals.mapValues{x: AnyRef => (x.toString + "(" + x.getClass + ")") }
	)
	
	// implement Binding:
	override def set(name: String, v: AnyRef) = iset(name, Val(v))
	override def get(name: String) = getVar(name).map(_.toJava).orNull
	override def contains(name: String) = 
		(locals contains name) || (parent contains name)
	override def setLocal(name: String, v: AnyRef) = setLocal(name, Val(v))
	final override def getNames: java.util.Set[String] = locals.keySet // only local names!
	final override def clear = locals.clear // won't affect any parent definitions!

	override def getParent: Resolver = parent.resolver
	override def setParent(r: Resolver) { parent = Parent(r) }

	// Creates a new scope that uses this one as parent.
	override def createSubContext = parent.inner(this)

	// internal set method
	final def iset(name: String, v: Val) = {
		val sv = preSetCheck(name, v)
		if (name.startsWith("_") || (locals contains name) )
			locals.update(name, sv)
		else if (!parent.set(name, sv)) locals.update(name, sv)
	}

	// Throws, if undefined!
	final def iget(name: String): Val =
		getVar(name).getOrElse(throw new Unresolved(name))

	override def copy = {
		// first determine parser to use:
		val newParser = parser match {
			case p: SelParser => new SelParser()
			case p: ConstParser => new ConstParser()
			case _ => throw new RuntimeException(
					"Unknown Parser: " + parser.getClass)
		}
		val cc = new CopyContext(newParser)
		// will perform actual work:
		copy(cc)
	}
	
	// ====================================================================
	// 
	//     Implementation details

	// Checks, whether this scope is stable
	def isStable = parent.stable
	// Checks, whether this scope is used to check stability
	def isStableCheck = parent.stableCheck

	// Retrieves either associated value or None
	def getVar(name: String): Option[Val] =
		locals.get(name).orElse( parent.getVar(name) )

	// Retrieves a value, that is either defined locally or
	// guaranteed to be constant
	def getConst(name: String): Option[Val] =
		locals.get(name).orElse( parent.getConst(name) )


	// Creates a stable subscpe to test the stability of some node
	def createStableCheck = {
		val cs = parent.inner(this)
		cs.parent = new StableCheck(this)
		cs
	}

	// Tests, if a node will evaluate stable within current scope.
	// If it doesn't, an exception will be thrown.
	def checkStability(node: INode) = createStableCheck.exec(node)

	// won't throw
	final def parseLocal(expression: String): Option[INode] = try {
		Some(parser.parse(expression, this))
	} catch { case _: Throwable =>	None}
	
	// Tests for local definition only
	final def defines(name: String) = locals contains name
	
	// fast track for accessing $
	final def setResult(v: Val) = {locals.update(RNAME, v); v}
	final def getResult: Val =
		locals.get(RNAME).orElse(parent.getVar(RNAME)).getOrElse(Vector.Empty)

	final def setLocal(name: String, v: Val) = locals.update(name, v)

	final def preSetCheck(name: String, v: Val) = {
		if (CONSTNAME.matches(name)) {
			if (this contains name)
				throw new EvalError("Cannot redefine constant " + name)

			// ensure, constant value is stable
			v.coerce match {
				case c: Callable if (!c.isStable) =>
					throw new EvalError(
						"Cannot assign instable function to constant " + name)
				case sv => sv
			}
		} else v
	}

	// Same as createSubContext, but used from inside
	def createInner = parent.inner(this)

	// Creates an execution point.
	// That means, the node is evaluated and the collapsed
	// result is stored within $.
	final def exec(node: INode) = {
		val v = node evalIn this
		setResult(v.coerce)
	}

	final def coerce(node: INode) = {
		val v = node evalIn this
		v.coerce
	}

	final def copy(cc: CopyContext): Scope = {
		cc.scopes.get(this) match {
			case Some(scope) => scope
			case _ => {
					val ns = new Scope(cc.parser)
					cc.scopes += this -> ns
					for ((n, v) <- locals){	ns.locals.update(n, v.copy(cc)) }
					ns.parent = parent.copy(cc)
					ns
				}
		}
	}

	// debugging output without displaying local values,
	// so closures won't cause circular output
	final def shallow: String =
		"[[ " + locals.keySet  + parent.shallow  + " ]] "

}
