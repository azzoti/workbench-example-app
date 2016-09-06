/*
 *  
 */

package see.values

/* BigInteger number.  */

import see.RangeOverflow

private[see] case class BigI(v: BigInt) extends IntLike
{
	type T =  BigInt
	override def selType = 'BigInt
	override def isType(typeId: Symbol) = (typeId == 'BigInt) || super.isType(typeId)

	override def toBool: Boolean = v.signum > 0
	override def toLong: Long = v.longValue
	override def toDouble: Double = v.doubleValue
	override def toBigI: BigInt = v
	override def toBig: BigDecimal = BigDecimal(v)
	def toJava = v.bigInteger

	override def propagate(other: Comparable) = other match {
		case BigR(_) => BigR(toBig)
		case Real(_) => BigR(toBig)
		case Str(_) => Str(toStr)
		case _ => this
	}
	override def cmp(rhs: Comparable) = v compare rhs.propagate(this).toBigI

	def abs = BigI(v.abs)
	def negate: Number = BigI(-v)
	override def add_ (rhs: Number) = BigI(v + rhs.toBigI)
	override def sub_ (rhs: Number) = BigI(v - rhs.toBigI)
	override def mul_ (rhs: Number) = BigI(v * rhs.toBigI)
	override def div_ (rhs: Number) = BigR(v) div_ rhs
	override def mod_ (rhs: Number) = BigR(v) mod_ rhs
	override def pwr_ (rhs: Number) = {
		try {
			// rhs should be integral if we're here
			if (rhs.toBigI > Long.MaxValue) // no way
				throw new RangeOverflow("BigInt exponent out of range.")
			if (rhs.toLong >= 0 && rhs.toLong < 1000000000)
				BigI(v.pow(rhs.toInt))
		} catch {
			case _: Throwable => // Retry with BigReal too, might still work due to scaling
		}
		BigR(toBig) pwr_ rhs
	}
	override def ~ = BigI(~v ) // ~v generates error for strange reason
	override def and_ (rhs: IntLike) = BigI( v & rhs.toBigI)
	override def or_ (rhs: IntLike) = BigI( v | rhs.toBigI)
	override def xor_ (rhs: IntLike) = BigI( v ^ rhs.toBigI)
	override def lsh_ (rhs: IntLike) = BigI( v << rhs.toInt)
	override def rsh_ (rhs: IntLike) = BigI( v >> rhs.toInt)
	override def gcd_ (rhs: IntLike) = BigI(v gcd rhs.toBigI)

	override def toString = v.toString + 'L'
	
	override def fits(destType: Class[_]) = 
		if (destType.isAssignableFrom(classOf[java.math.BigInteger])) 10 
	else 0

}

