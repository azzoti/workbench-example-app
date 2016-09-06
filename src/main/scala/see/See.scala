/*
 * See.scala
 *
 */

package see

import see.values._


/** Interface describing an evaluation result.
 * Pure trait, so we can use it from Java.
 *
 */
trait IResult {
	/** Forces result into java boolean.
	 * Guaranteed to succeed.
	 */
	def toBool: Boolean

	/** Forces result into a reparsable Java string representation.
	 */
	def toStr: String
	/** Forces result into Java long, if possible.
	 */
	def toLong: Long
	/** Forces result into Java double, if possible.
	 */
	def toDouble: Double
	def toBigI: BigInt
	/** Forces result into Java BigDecimal, if possible.
	 */
	def toBig: BigDecimal

	/** Converts a See result into its Java representation, if any.
	 * Consult the Sel documentation to see which Java class will be generated
	 * from any specific type.
	 *
	 * If no Java equivalent exists for a given Sel result,
	 * an EvalException will be thrown.
	 */
	def toJava: AnyRef

	/** Number of contained elements, if any. */
	def size: Int

	def dump: String
}


/** Pure interface that describes a node returned by the parser.
 *
 * The parser will return objects that implement this interface, 
 * so it is possible to evaluate an expression multiple times without 
 * need to parse it every time.
 * 
 * In general, it should not be necessary to know any details
 * about such an instance when using it from Java.
 */
trait INode {
	private [see] def evalIn(s: Scope): Val
    private [see] def simplifyIn(s: Scope): INode
    private [see] def isDefinedIn(s: Scope): Boolean
}

/** The See main class.
 *
 * This one must be created by embedding applications.
 */
abstract class See extends Binding {

	// If we define this as trait See, See.create() will not work from Java!

	/** Parses the given expression into node tree.
	 * @param expression Expression to parse.
	 * @return Node tree for evaluation.
	 * @throws ParseException, if expression is not a valid Sel expression.
	 */
	def parse(expression: String): INode

	/** Evaluates a pre-parsed node tree.
	 * @param node Node returned by parse().
	 * @return Some kind of See result.
	 * For use within Java, you will probably want to
	 * call [[see.IResult#toJava]] upon it.
	 *
	 * In most cases, it is simpler to use one of of the convenience
	 * methods instead.
	 * However, any result may be assigned to a Sel variable using
	 * [[see.Resolver#set]], even if no Java equivalent exists.
	 */
	def eval(node: INode): IResult

	/** Convenience method for {{{setLocal(name, new Long(v)) }}}*/
	def setLocal(name: String, v: Int)
	/** Convenience method for {{{setLocal(name, new Double(v)) }}}*/
	def setLocal(name: String, v: Double)
	/** Convenience method for {{{setLocal(name, new Boolean(v)) }}}*/
	def setLocal(name: String, v: Boolean)

	/** Convenience method for {{{ set(name, new Long(v)) }}}
	 */
	final def set(name: String, v: Int) { iset(name, Lint(v)) }
	/** Convenience method for {{{ set(name, new Double(v)) }}} */
	final def set(name: String, v: Double) { iset(name, Real(v)) }
	/** Convenience method for {{{ set(name, new Boolean(v)) }}} */
	final def set(name: String, v: Boolean) { iset(name, Bool(v)) }
	private [see] def iset(name: String, v: Val)

	/** Parses the given expression and evaluates the resulting node tree.
	 * This is just a conveninence method for
	 * {{{ eval(parse(expression)) }}}
	 */
	def eval(expression: String): IResult = eval(parse(expression))

	/** Evaluates node and converts result into double.
	 */
	def evalAsDouble(node: INode): Double = eval(node).toDouble

	/** Evaluates node and converts result into long.
	 */
	def evalAsLong(node: INode): Long = eval(node).toLong

	/** Evaluates node and converts result into boolean.
	 */
	def evalAsBoolean(node: INode): Boolean = eval(node).toBool

	/** Evaluates node and converts result into String.
	 */
	def evalAsString(node: INode): String = eval(node).toStr

	/** Evaluates node just converting the result into its Java representation.
	 * This method must be called, if a non-primitive result is expected.
	 * It will still throw an EvalException, if evaluation itself fails,
	 * but not for any conversion failures.
	 */
	def evalAsObject(node: INode): AnyRef = eval(node).toJava

	def evalAsDouble(expr: String): Double = evalAsDouble(parse(expr))
	def evalAsLong(expr: String): Long = evalAsLong(parse(expr))
	def evalAsBoolean(expr: String): Boolean = evalAsBoolean(parse(expr))
	def evalAsString(expr: String): String =  evalAsString(parse(expr))
	def evalAsObject(expr: String): AnyRef = evalAsObject(parse(expr))


	/** Creates a new See context that may access the bindings of its parent.
	 * Changing any non-local definition will also affect the parent context.
	 * @return A See context that is a subcontext of this one.
	 */
	def createSubContext: See

	/** Creates an independent copy of this scope.
	 * @return A See context that can be used independent from the
	 * original one, e.g within a different thread.
	 * It will not contain any references to its source, but still uses the
	 * same resolver, if any.
	 */
	def copy: See
}

/** See factory methods.
 */
object See {
	
	/** Convenience method for scala usage only. Allows val see = See().
	 */
	def apply(): See = new Scope(new SelParser(), NoParent)

	/** Creates a See context that will only accept simple constant expressions.
	 * Any variable assignment, function definition etc. will cause a parsing error.
	 */
	def createConst: See = new Scope(new ConstParser(), StableScope)

	/** Creates a See context that supports all Sel features.
	 *
	 * This implies that it is possible to generate nodes which
	 * cannot be evaluated in finite time.
	 */
	def create(): See = new Scope(new SelParser(), NoParent)

	/** Returns version as string.
	 * @return Version string in the form "<major>.<minor>.<patch>".
	 * E.g. "1.2.0"
	 */
	def getVersion = "1.3.1"

	private[see] var reflectionEnabled = false
	

}

