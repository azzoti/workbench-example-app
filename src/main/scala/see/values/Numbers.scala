/*
 *  
 */

package see.values

/* A generic number. */
private[see] abstract class Number extends Comparable
{
	override def selType = 'Number
	override def isType(typeId: Symbol) = (typeId =='Number) || super.isType(typeId)

	def negate: Number
	def abs: Number
	protected def add_(rhs: Number): Number
	protected def sub_(rhs: Number): Number
	protected def mul_(rhs: Number): Number
	protected def div_(rhs: Number): Number
	protected def mod_(rhs: Number): Number
	protected def pwr_(rhs: Number): Number

	// Ensures, operation will be performed upon richest necessary type.
	// We could probably use some intrinsics mechanism instead, but
	// first I'm not fond of that mechanism at all, and second it
	// still would not allow e.g n1 + n2 with n1: Number, n2: Number
	private final def np(n: Number) = propagate(n).asInstanceOf[Number]
	final def + (rhs: Number) = np(rhs).add_(rhs)
	final def - (rhs: Number) = np(rhs).sub_(rhs)
	final def * (rhs: Number) = np(rhs).mul_(rhs)
	final def / (rhs: Number) = np(rhs).div_(rhs)
	final def % (rhs: Number) = np(rhs).mod_(rhs)
	final def ** (rhs: Number)= np(rhs).pwr_(rhs)

	// for convenience:
	def toInt = toLong.toInt
}

private[see] object Number {
    implicit object ord extends Ordering[Number] {
	def compare(l: Number, r: Number): Int = Comparable(l,r)
}
}


/* Defines bitwise operations and shifts */
private[see] abstract class IntLike extends Number {
	override def selType = 'Integral
	override def isType(typeId: Symbol) =
		(typeId == 'Integral) || super.isType(typeId)

	def ~ : IntLike
	protected def and_(rhs: IntLike): IntLike
	protected def or_(rhs: IntLike): IntLike
	protected def xor_(rhs: IntLike): IntLike
	protected def lsh_(rhs: IntLike): IntLike
	protected def rsh_(rhs: IntLike): IntLike
	protected def gcd_(rhs: IntLike): IntLike

	private final def ip(n: Number) = propagate(n).asInstanceOf[IntLike]
	final def & (rhs: IntLike) = ip(rhs).and_(rhs)
	final def | (rhs: IntLike) = ip(rhs).or_(rhs)
	final def ^ (rhs: IntLike) = ip(rhs).xor_(rhs)
	final def << (rhs: IntLike) = ip(rhs).lsh_(rhs)
	final def >> (rhs: IntLike) = ip(rhs).rsh_(rhs)
	final def gcd (rhs: IntLike) = ip(rhs).gcd_(rhs)
}

