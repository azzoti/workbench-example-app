/*
 *  
 */

package see.values

import see.CopyContext
import see.Scope

/* A value that serves as container for other values. */
private[see] abstract class Container
extends Value with Callable {

	override def selType = 'Container
	override def isType(typeId: Symbol) =
		(typeId == 'Container) || super.isType(typeId)

	def addLeft(v: Val): Container
	def addRight(v: Val): Container
	def isEmpty: Boolean
	def at(index: Val): Val
	def foreach(v: Val => Unit): Unit
	def map(mapf: Val => Val): Container
	def forall(pred: Val => Boolean): Boolean
	def deepCmp(rhs: Val): Boolean

	def vals: Iterator[Val]
	def assocs: Iterator[(Val,Val)]
	def contains(rhs: Val): Boolean // works on keys in general!
	def containsStrong(rhs: Val): Boolean // works on keys in general!
	def containsWeak(rhs: Val): Boolean // works on keys in general!
	def union(rhs: Val): Container
	def intersect(rhs: Val): Container
	def diff(rhs: Val): Container

	// Note that a container is forced to false,
	// if ANY element evaluates to false!
	override def toBool: Boolean = !isEmpty && forall(_.coerce.toBool)
	override def toLong: Long = size

	override def isDefinedIn(s: Scope) = forall{
		v: Val => v match {
			case c: Callable => c.isDefinedIn(s)
			case _ => true
		}
	}

	override def isStable = forall{
		v: Val => v match {
			case c: Callable => c.isStable
			case _ => true
		}
	}

	override def coerce = map( _.coerce )
	override def copy(cc: CopyContext) = map( _.copy(cc) ) 

	override def isEqualTo(other: Val): Boolean = {
		if (size != other.size) return false
		val l = coerce.asInstanceOf[Container]
		val r = other.coerce
		(l eq r) || (l deepCmp r)
	}

	def isFlat = forall(_.coerce.isInstanceOf[Scalar])
//	def reduce(accu: Val)(f: (Val, Val) => Val): Val =
//		(accu.coerce /: map(_.coerce))(f)

	override def call(s: Scope, args: Val) = args match {
		case _ => map(v => v match {
					case c: Callable => c.call(s, args)
					case x => x
				}
			)
	}

}

