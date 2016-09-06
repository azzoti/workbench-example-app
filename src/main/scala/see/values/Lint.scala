/*
 *  
 */

package see.values

/* Integral number. We always use long to represent these. */
import see.DivisionByZero

private[see] case class Lint(v: Long) extends IntLike
{
	type T =  Long
	override def selType = 'Int
	override def isType(typeId: Symbol) = (typeId == 'Int) || super.isType(typeId)

	def toJava = Long.box(v)

	override def toBool: Boolean = v > 0
	override def toLong: Long = v
	override def toDouble: Double = v
	override def toBigI: BigInt = BigInt(v)
	override def toBig: BigDecimal = BigDecimal(v)

	override def propagate(other: Comparable) = other match {
		case Lint(_) => this
		case Str(_) => Str(toStr)
		case Real(_) => Real(v)
		case BigI(_) => BigI(toBigI)
		case BigR(_) => BigR(toBig)
	}

	override def cmp(rhs: Comparable) =  v compare rhs.propagate(this).toLong

	def abs = Lint(v.abs)
	def negate = Lint(-v)
	override def add_ (rhs: Number) = Lint(v + rhs.toLong)
	override def sub_ (rhs: Number) = Lint(v - rhs.toLong)
	override def mul_ (rhs: Number) = Lint(v * rhs.toLong)
	override def div_ (rhs: Number) = {  val r = rhs.toLong
									   if (r == 0) throw new DivisionByZero()
									   else if (v % r == 0) Lint(v / r)
									   else Real(v.toDouble / r)
	}

	override def mod_ (rhs: Number) = { val r = rhs.toLong
									   if (r == 0) throw new DivisionByZero()
									   else Lint(v % r)
	}
	override def pwr_ (rhs: Number) = Lint(math.pow(v, rhs.toLong).toLong)

	override def ~ = Lint(~v)
	override def and_ (rhs: IntLike) = Lint( v & rhs.toLong)
	override def or_ (rhs: IntLike) = Lint( v | rhs.toLong)
	override def xor_ (rhs: IntLike) = Lint( v ^ rhs.toLong)
	override def lsh_ (rhs: IntLike) = {
		val r = rhs.toLong
		// due to strange compiler behavior, we have to check shift first:
		Lint( if (math.abs(r) >= 64) 0
			 else if (r >= 0) v << r else v >>> -r)
	}

	override def rsh_ (rhs: IntLike) = {
		val r = rhs.toLong
		Lint( if (r >= 0) v >> r else v << -r)
	}
	// for simplicty, we use BigInt:
	override def gcd_ (rhs: IntLike) = Lint((toBigI gcd rhs.toBigI).toLong)

	override def fits(destType: Class[_]) = if (
		(destType == java.lang.Long.TYPE) ||
		destType.isAssignableFrom(classOf[java.lang.Long])) 10 
	else if (
		(destType == java.lang.Integer.TYPE) ||
		destType.isAssignableFrom(classOf[java.lang.Integer])) 8
	else if (
		(destType == java.lang.Short.TYPE) ||
		destType.isAssignableFrom(classOf[java.lang.Short])) 6
	else if ((destType == java.lang.Byte.TYPE) ||
			 destType.isAssignableFrom(classOf[java.lang.Byte])) 4
	else 0

}

private[see] object Lint{
	val Zero = Lint(0)
	val One = Lint(1)
}
