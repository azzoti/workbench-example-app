/*
 * Values.scala
 * Defines generic See types
 */

package see.values

import see.CopyContext
import see.EvalError
import see.Scope
import see.Unsupported
import see.IResult
import scala.util.parsing.combinator._
import see.nodes._


/** Superclass for all values, we can work with.
 */
private[see] abstract class Val extends IResult {
	def selType: Symbol  // type name as seen by Sel
	def isType(typeId: Symbol) = false
	// the first subclass within each branch has to override these with:
	// override def isType(typeId: Symbol) = (typeId == 'selType) || super.isType(typeId)

	def toBool: Boolean = false
	override def toStr: String = toString // not reparsable
	override def toLong: Long = throw new EvalError(
		"Conversion to long not possible for " + selType + ".")
	override def toDouble: Double = throw new EvalError(
		"Conversion to double not possible for " + selType + ".")
	override def toBigI: BigInt = throw new EvalError(
		"Conversion to BigInteger not possible for null.")
	override def toBig: BigDecimal = throw new EvalError(
		"Conversion to BigDecimal not possible for " + selType + ".")

	def coerce: Val = this
	
	// Copies value into new scope.
	// This implementation is good for all immutable values.
	// Must be overwritten for closures and containers 
	// that might contain closures.
	def copy(cc: CopyContext) = this

	// similar to equals(), but doesn't require same type.
	def isEqualTo(other: Val): Boolean = {
		// coerce here in any case to avoid the strangest effects,
		//  e.g. true(c == c) and true == (c > c) at the same time
		//  we might get false == (c == c) instead, but that is at least consequent
		val l = coerce
		val r = other.coerce
		if ((l eq this) && (r eq other)) false
		else (l eq r) || (l isEqualTo r)
	}

	def dump = toString
	def fits(destType: Class[_]) = 0 // no fit by default

	def convertTo(destType: Class[_]): AnyRef = {
		if (destType.isPrimitive)
			convertToPrimitive(destType).asInstanceOf[AnyRef]
		else if (classOf[Val] isAssignableFrom destType ) // leave any Val as is
			this
		else convertToObject(destType)
	}
		
	def convertToPrimitive(destType: Class[_]): Any = destType match {
		case java.lang.Boolean.TYPE		=> toBool
		case java.lang.Long.TYPE		=> toLong
		case java.lang.Double.TYPE		=> toDouble
		case java.lang.Float.TYPE		=> toDouble.floatValue
		case java.lang.Integer.TYPE		=> toLong.intValue
		case java.lang.Byte.TYPE		=> toLong.byteValue
		case java.lang.Short.TYPE		=> toLong.shortValue
		case java.lang.Character.TYPE	=> toLong.toChar // probably not a  good idea
//		case java.lang.Void.TYPE		=> Unit.asInstanceOf[ClassManifest[T]]
		case _ => throw new EvalError(
				"Unsupported primitive conversion to '"+ destType.getName +"'.")
	}				
	
	def convertToObject(destType: Class[_]): AnyRef = {
		if (destType.isAssignableFrom(classOf[String]))
			return toStr
		else if (destType.isAssignableFrom(classOf[java.lang.Boolean]))
			return toBool.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.lang.Long]))
			return toLong.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.lang.Integer]))
			return toLong.intValue.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.lang.Double]))
			return toDouble.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.lang.Float]))
			return toDouble.floatValue.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.lang.Short]))
			return toLong.shortValue.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.lang.Byte]))
			return toLong.byteValue.asInstanceOf[AnyRef]
		else if (destType.isAssignableFrom(classOf[java.math.BigDecimal]))
			return toBig
		else if (destType.isAssignableFrom(classOf[java.math.BigInteger]))
			return toBigI
		// anything else must be specialized
		throw new EvalError(
			"Cannot convert '" + getClass.getName + 
			"' into '" + destType.getName + "'.")
	}
}

private[see] object Val{
	
	/** Performs translation from Java into Sel types.
	 */
	final def apply(a: AnyRef): Val = a match {
		case v: Val => v
		case v: java.math.BigInteger => BigI(new BigInt(v))
		case v: java.math.BigDecimal => BigR(BigDecimal(v))
		case v: java.lang.Double => Real(v.doubleValue)
		case v: java.lang.Float => Real(v.doubleValue)
		case v: java.lang.Boolean => Bool(v.booleanValue)
			// fine for byte, short, int, long
			// but may cause trouble for more complicated types, e.g some complex
		case v: java.lang.Number => Lint(v.longValue)

		case v: java.lang.String => Str(v)
		case v: java.util.regex.Pattern => Rexp(v)
		case a: Array[AnyRef] => Vector.fromJava(a)
		case null => NullVal
		case x => AnyVal(x)
	}
}


/** Wrapper for unknown types.
 */
private[see] case class AnyVal(any: AnyRef) extends Val {
	override def selType = Symbol(any.getClass.toString)
	override def isType(typeId: Symbol) = typeId == selType // no subclasses we know of

	override def size = 1
	override def toJava = any

	override def isEqualTo(other: Val) = this.equals(other)
	
	override def fits(destType: Class[_]) = 
		if (destType.isInstance(any)) 10 else 0

	override def convertTo(destType: Class[_]) = {
		if (destType.isInstance(any)) any 
		else throw new EvalError(
			"Cannot convert '" + any.getClass.getName + 
			"' to '" + destType.getName + "'.")
	}
}

/** Wrapper for null.
 */
private[see] case object NullVal extends Val {
	override def selType = 'Null
	// type compare never matches!

	override def toStr: String = "Null Reference"
	override def toLong: Long = throw new EvalError(
		"Conversion to long not possible for null.")
	override def toDouble: Double = throw new EvalError(
		"Conversion to double not possible for null.")
	override def toBigI: BigInt = throw new EvalError(
		"Conversion to BigInteger not possible for null.")
	override def toBig: BigDecimal = throw new EvalError(
		"Conversion to BigDecimal not possible for null.")

	override def size = 0
	override def toJava = null
	override def coerce: Val = throw Unsupported(null)
	override def isEqualTo(other: Val) = false
	
	// may crash!
	override def convertTo(destType: Class[_]) = null.asInstanceOf[AnyRef]
}

/** Wrapper for void (May be returned form a native call).
 */
private[see] case object VoidVal extends Val {
	override def selType = 'Void
	// type compare never matches!

	override def toStr: String = "Void"

	override def size = 0
	override def toJava = null // ?
	override def coerce: Val = this
	override def isEqualTo(other: Val) = false
	
	override def convertTo(destType: Class[_]) = Unit
}


/** Symbols are fixed names that won't show up as variables.
 * The parser recognizes them as constants, but that's it'
 */
private[see] case class SymVal(sym: Symbol) extends Val {
	override def selType: Symbol = 'Symbol
	override def isType(typeId: Symbol) = typeId == selType // no subclasses

	override def toString: String = "Symbol("+sym.name+")"
	override def toStr: String = sym.name

	override def size = 1
	override def toJava = this

	override def isEqualTo(other: Val) = other match {
		case SymVal(z) => sym == z
		case _ => false
	}

	override def fits(destType: Class[_]) = 
		// No full match; we would prefer a String
	if (destType.isAssignableFrom(classOf[String])) 5 else 0
}

/** Marks a value that should never show up as final evaluation result.
 * This means its coerce method will alwas result in a different Val type.
 * /
 private [see] trait Transient {
 self: Val =>
 override def size = coerce.size
 override def selType = coerce.selType
 override def isType(typeId: Symbol) =  coerce.isType(typeId)
 override def toBool = coerce.toBool
 override def toStr = coerce.toStr
 override def toLong = coerce.toLong
 override def toDouble = coerce.toDouble
 override def toBigI = coerce.toBigI
 override def toBig = coerce.toBig
 override def toJava = coerce.toJava
 override def copy(cc: CopyContext) = coerce.copy(cc)
 override def fits(destType: Class[_]) = coerce.fits(destType)
 override def convertTo(destType: Class[_]) = coerce.convertTo(destType)
 override def dump = "<transient " + coerce.dump + ">"
 }


 / ** A reference to some other type. 
 * These are typically transient.
 * /
 private[see] abstract class Reference extends Val with LValue {
 override def selType = 'Reference
 override def isType(typeId: Symbol) = 
 (typeId == 'Reference) || super.isType(typeId)
 }
 */


/* A Value type. */
private[see] abstract class Value extends Val {
	override def selType = 'Value
	override def isType(typeId: Symbol) = (typeId == 'Value) || super.isType(typeId)
}

/* A generic scalar value. */
private[see] abstract class Scalar extends Value
{
	type T
	val v: T
	override def selType = 'Scalar
	override def isType(typeId: Symbol) = (typeId == 'Scalar) || super.isType(typeId)

    override def toString = v.toString
	override def toStr = toString // fine in most cases
	override def size = 1
}


/* The result of a boolean operation. */
private[see] object Bool
{
	private val myType = 'Bool
	val False = Bool(false)
	val True = Bool(true)
}
private[see] case class Bool(v: Boolean) extends Scalar
{
	type T = Boolean
	override def selType = Bool.myType
	override def isType(typeId: Symbol) = (typeId == Bool.myType) || super.isType(typeId)

	override def toBool = v
	override def toJava = if(v) java.lang.Boolean.TRUE
	else java.lang.Boolean.FALSE

	override def isEqualTo(other: Val) = other match {
		case Bool(z) => v == z
		case _ => false
	}
	
	override def fits(destType: Class[_]) = 
		if((destType == java.lang.Boolean.TYPE) ||
		   destType.isAssignableFrom(classOf[Boolean])) 10 else 0
}

/* A generic comparable  value. */
private[see] abstract class Comparable extends Scalar with Ordered[Comparable]
{
	override def selType = 'Comparable
	override def isType(typeId: Symbol) = (typeId == 'Comparable) || super.isType(typeId)
	
	// Converts this into some type that can take up both this and other.
	def propagate(other: Comparable): Comparable
	def cmp(rhs: Comparable): Int
	// required by Ordered
	override def compare(rhs: Comparable) = cmp(rhs)

	override def isEqualTo(other: Val) = other match {
		case c: Comparable => Comparable(this, c) == 0
		case _ => false
	}
}

private[see] object Comparable extends Ordering[Comparable] {

	def compare(l: Comparable, r: Comparable): Int = {
		// If any operand is number, use number comparison whenever possible!
		l match {
			case n: Number => (r propagate l) match {
					case rn: Number => (l propagate rn) cmp rn
					case _ => (l propagate r) cmp r // must use r's compare
				}
				// in all other cases, this should work fine
				// we don't have anything left anyway ...
			case _ => (l propagate r) cmp r
		}
	}

	def apply(l: Comparable, r: Comparable) = compare(l,r)
}

private[see] trait Callable {
	// not typecheckable

	def call(s: Scope, args: Val): Val
	def isDefinedIn(s: Scope): Boolean
	def isStable: Boolean
}