/*
 *
 */

package see.values

import see.DivisionByZero


/* Floating point number (always double). */
private[see] case class Real(v: Double) extends Number {
	type T = Double
	override def selType = 'Real
	override def isType(typeId: Symbol) = (typeId == 'Real) || super.isType(typeId)

	override def toJava = Double.box(v)

	override def propagate(other: Comparable) = other match {
		case BigI(_) => BigR(toBig)
		case BigR(_) => BigR(toBig)
		case Str(_) => Str(toStr)
		case _ => this
	}
	override def toBool: Boolean = v > 0
	override def toLong: Long = v.toLong
	override def toDouble: Double = v
	override def toBigI: BigInt = BigDecimal(v).toBigInt
	override def toBig: BigDecimal = BigDecimal(v)

	override def cmp(rhs: Comparable) = v compare rhs.propagate(this).toDouble

	private def reduce(result: Double) = {
		if (result == result.toLong)
			Lint(result.toLong)
		else Real(result)
	}

	def abs = Real(v.abs)
	def negate: Number = Real(-v)
	override def add_ (rhs: Number) = {
		val r = v + rhs.toDouble
		if (r.isInfinite)
			BigR(v) add_ rhs
		else Real(r)
	}
	override def sub_ (rhs: Number) = {
		val r = v - rhs.toDouble
		if (r.isInfinite)
			BigR(v) sub_ rhs
		else Real(r)
	}
	override def mul_ (rhs: Number) = {
		val r = v * rhs.toDouble
		if (r.isInfinite)
			BigR(v) mul_ rhs
		else Real(r)
	}
	override def div_ (rhs: Number) = {
		val r = rhs.toDouble
		if (r == 0.0) throw new DivisionByZero()
		else {
			val result  = v / r
			if (result.isInfinite) BigR(v) div_ rhs
			else reduce (result)
		}
	}
	override def mod_ (rhs: Number) = {
		// no overflow, possible here
		if (rhs.toDouble == 0.0) throw new DivisionByZero()
		else reduce (v % rhs.toDouble)
	}
	override def pwr_ (rhs: Number) = Real(math.pow(v, rhs.toDouble))
	
	override def fits(destType: Class[_]) = if(
		(destType == java.lang.Double.TYPE) ||
		destType.isAssignableFrom(classOf[java.lang.Double])) 10 
	else if ((destType == java.lang.Float.TYPE) ||
			 destType.isAssignableFrom(classOf[java.lang.Float])) 5 
	else 0
	
}
