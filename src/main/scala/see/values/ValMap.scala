/*
 *  
 */

package see.values

import see.ParamError
import see.operations.At

private[see] case class ValMap(vmap: Map[Val, Val])
extends Container {
	type VM = Map[Val, Val]

	override def selType = 'Map
	override def isType(typeId: Symbol) =
		(typeId == 'Map) || super.isType(typeId)

	override def size = vmap.size
	override def isEmpty = vmap.isEmpty
	override def at(index: Val): Val = index match{
		case Vector(h::t) => At(at(h), Vector(t))
		case other => vmap.getOrElse( 
				other, {
					throw new ParamError("Unknown key: " + index)}
			)}

	def vals = vmap.keysIterator
	def assocs = vmap.iterator
	def contains(rhs: Val) = containsStrong(rhs)
	def containsWeak(rhs: Val) = vmap.keys.exists(_.isEqualTo(rhs))
	def containsStrong(rhs: Val) = vmap.keys.exists(_ == rhs)
	def union(rhs: Val) = rhs match {
		case c: Container => ValMap(vmap ++ c.assocs)
		case x => if (contains(x)) this else ValMap(vmap + (x -> x))
	}

	def intersect(rhs: Val) = rhs match {
		case c: Container => ValMap(vmap.filterKeys(c.contains(_)))
		case x => if (contains(x)) ValMap(Map(x -> vmap(x))) else ValMap.Empty
	}
	def diff(rhs: Val) = rhs match {
		case c: Container => ValMap(vmap.filterKeys(!c.contains(_)))
		case x => ValMap(vmap.filterKeys(rhs != _))
	}


	override def addLeft(v: Val) = ValMap( v match {
			case ValMap(vm) => vmap ++ vm
			case a: Assoc => vmap + (a.key -> a.value)
			case x => vmap + (x -> x)
		})
	override def addRight(v: Val) = addLeft(v)
	override def foreach(v: Val => Unit) = vmap.values.foreach(v)
	override def map(mapf: Val => Val) = ValMap(vmap.mapValues( mapf ))

	override def forall(pred: Val => Boolean) = vmap.values.forall(pred)

	override def deepCmp(rhs: Val): Boolean = rhs match {
		case ValMap(rs) => {
				for (kv <- vmap){
					val vr = rs.get(kv._1)
					if ((vr == None) || !(kv._2 isEqualTo vr.get)) return false
				}
				true
			}
		case _ => false
	}


    override def toString = vmap.mkString("Map(", ", ", ")")
	override def toStr: String = vmap.mkString("map(", ", ", ")")

	override def toJava = this // might generate TreeMap
}

private[see] object ValMap {

	// comes to void as near as we may get
	object Empty extends ValMap(Map()) {
	    override def toString = "EmptyMap"

		override def toBool: Boolean = false
		override def toLong: Long = 0
		override def size: Int = 0
		override def isFlat = true
		override def map(mapf: Val => Val) = this
	}

	private def assoc(key: Val, value: Val) = key match {
		// Using a whole vector as key doesn't make sense,
		// because subscripting with a vector argument doesn't work anyway.
		// So we interpret it as vector of keys
		case Vector(ks) => {
				value match {
					case Vector(vs) =>
						for ((k,v) <- ks zip vs) yield k -> v
					case _ =>
						for (k <- ks) yield k -> value
				}
			}
		case _ => List((key,value))
	}


	def apply(init: Val): ValMap = {
		val mb = Map.newBuilder[Val, Val]
		init match {
			case a: Assoc => mb ++= assoc(a.key, a.value)
			case Vector(vs) => {
					if (vs.isEmpty) return Empty // ensure empty maps compare equal.

					for (v <- vs) {v match {
							case a: Assoc => mb ++= assoc(a.key, a.value)
							case x => mb ++= assoc(x, x)
						}
					}
					mb.result
				}
			case x => mb ++= assoc(x, x) // a set entry
		}
		new ValMap(mb.result)
	}

}
