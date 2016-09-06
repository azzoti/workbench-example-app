/*
 *  
 */

package see.values

import java.math.MathContext
import java.math.RoundingMode
import java.math.{BigDecimal => JBD}
import see.DivisionByZero
import see.ParamError
import see.RangeOverflow


/* BigDecimal. */
private[see] object BigR {
	def apply(bi: BigInt): BigR = BigR(BigDecimal(bi))
	val Zero = BigDecimal(JBD.ZERO)
	val One = BigDecimal(JBD.ONE)
	val MC_ROUND = MathContext.UNLIMITED
	val MC_CEIL = new MathContext(0, RoundingMode.CEILING)
	val MC_FLOOR =  new MathContext(0, RoundingMode.FLOOR)
	val MAX_EXP = BigDecimal(999999999)

	def isInteger(bd: BigDecimal) = (bd remainder One) == Zero

}

private[see] case class BigR(v: BigDecimal) extends Number {
	type T = BigDecimal
	override def selType = 'BigReal
	override def isType(typeId: Symbol) = (typeId == 'BigReal) || super.isType(typeId)

	override def toString = v.toString + 'L'


	override def toBool: Boolean = v.signum > 0
	override def toLong: Long = v.toLongExact
	override def toDouble: Double = v.toDouble
	override def toBigI: BigInt = v.toBigInt
	override def toBig: BigDecimal = v
	def toJava = v.bigDecimal

	override def propagate(other: Comparable) = other match {
		case Str(_) => Str(toStr)
		case _ => this
	}

	override def cmp(rhs: Comparable) = v compare rhs.propagate(this).toBig

	def abs = BigR(v.abs)
	def negate = BigR(-v)
	override def add_ (rhs: Number) = BigR(v + rhs.toBig)
	override def sub_ (rhs: Number) = BigR(v - rhs.toBig)
	override def mul_ (rhs: Number) = BigR(v * rhs.toBig)
	override def div_ (rhs: Number) = {
		val r = rhs.toBig
		if (r == BigR.Zero) throw new DivisionByZero()
		else if ((v remainder r) == BigR.Zero)
			BigI( v.quot(r).toBigInt)
		else
			BigR(v/r)
	}

	override def mod_ (rhs: Number) = {
		val r = rhs.toBig
		if (r == BigR.Zero) throw new DivisionByZero()
		else {
			val result = v % r
			if (BigR.isInteger(result))
				BigI( result.toBigInt)
			else
				BigR(result)
		}
	}

	override def pwr_ (rhs: Number) = {
		val exp = rhs.toBig
		var result: BigDecimal = 0
		if (exp.abs > BigR.MAX_EXP ) // no way
			throw new RangeOverflow("BigReal exponent out of range.")
		if ((exp remainder BigR.One) == BigR.Zero) try {// can use pow
			val iexp = exp.toInt
			if (exp.signum >= 0)
				result = v.pow(iexp)
			else // limited context required for negative exponents
				result = BigDecimal(v.bigDecimal.pow(iexp, MathContext.DECIMAL64))
		} catch {
			case _: Throwable => throw new ParamError("BigReal exponent error")
		}
		else {
			// rational number, we need to go down to Real.
			val rexp = exp.toDouble
			if (rexp.isInfinite) // no way
				throw new RangeOverflow("BigReal exponent out of range.")

			// +2 : small bias for values > 1 gives better results.
			val scale = v.scale - v.precision + 2
			val vnorm = v.bigDecimal.scaleByPowerOfTen(scale)
			val mant = vnorm.doubleValue
			val f1 = math.pow(mant, rexp)
			if (f1.isInfinite) // no way to get back
				throw new RangeOverflow("BigReal exponent out of range.")
			val f2 = math.pow(10, rexp * -scale)
			if (f2.isInfinite) // no way to get back
				throw new RangeOverflow("BigReal exponent out of range.")
			// Note that using a higher precision makes things worse due to
			// calculation inaccuracy
			result = BigDecimal(f1,MathContext.DECIMAL32) *
			BigDecimal(f2,MathContext.DECIMAL32)

		}
		if (BigR.isInteger(result))
			BigI( result.toBigInt)
		else
			BigR(result)
	}

	override def fits(destType: Class[_]) = 
		if(destType.isAssignableFrom(classOf[java.math.BigDecimal]))10 
	else 0
	
}

