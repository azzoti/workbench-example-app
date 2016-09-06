/*
 *
 */

package see.values

import see.Illegal

private[see] case class Assoc(key: Val, value: Val)
extends Container {

	override def selType = 'Assoc
	override def isType(typeId: Symbol) =
		(typeId == 'Assoc) || super.isType(typeId)

	override def size= 1
	override def isEmpty = false
	override def at(index: Val) = value

	override def vals = List(value).iterator
	override def assocs = List((key, value)).iterator
	def contains(rhs: Val) = containsStrong(rhs)
	def containsWeak(rhs: Val) = key.isEqualTo(rhs)
	def containsStrong(rhs: Val) = key == rhs
	def union(rhs: Val) = rhs match {
		case m: ValMap => m.union(this)
		case v: Vector => v.union(this)
		case a: Assoc => new ValMap(Map(key->value, a.key -> a.value))
		case x => new ValMap(Map(key->value, x -> x))
	}
	def intersect(rhs: Val) = rhs match {
		case m: ValMap => m.intersect(this)
		case v: Vector => v.intersect(this)
		case a: Assoc => if (key.isEqualTo(a.key)) this else ValMap.Empty
		case x => if (key.isEqualTo(x)) this else ValMap.Empty
	}
	def diff(rhs: Val) = rhs match {
		case c: Container => if (c contains key) ValMap.Empty else this
		case x => if (key == x) ValMap.Empty else this
	}

	override def addLeft(v: Val) = Assoc(key, Vector(List(v, value)))
	override def addRight(v: Val) = Assoc(key, Vector(List(value, v)))
	override def foreach(v: Val => Unit) = v(value)
	override def map(mapf: Val => Val) = Assoc(key, mapf(value))
	override def forall(pred: Val => Boolean) = pred(value)
	override def deepCmp(rhs: Val): Boolean = rhs match {
		case Assoc(rk, rv) => (key isEqualTo rk) && (value isEqualTo rv)
		case _ => false
	}


	override def toString = "Assoc(" + key + " -> " + value + ")"
	override def toStr = key.coerce.toStr + " -> " + value.coerce.toStr
	override def toJava = this
	override def coerce = {
		val cv = value.coerce;
		if (cv eq value) this
		else Assoc(key, cv)
	}


}
