/*
 *  
 */

package see.values

/* Vector type. May contain any value as elements, even other vectors. */
import see.EvalError
import see.Unresolved
import see.operations.At

private[see] case class Vector(init: Seq[Val])
extends Container
{
	override def selType = 'Vector
	override def isType(typeId: Symbol) = (typeId == 'Vector) || super.isType(typeId)

	val values = init.toList

	override def size= values.size
	override def isEmpty = values.isEmpty
	override def at(index: Val) = index match {
		case n: Number => {
				val i = n.toInt
				if (i >= values.size || i < 0 && -i > values.size)
					throw new Unresolved("Vector index out of bounds:" + i)
				else if (i < 0)
					values(values.size + i)
				else values(i)
			}
		case Vector(h :: t) => At(at(h), Vector(t))
		case _ => throw new Unresolved("Illegal vector index:" + index)
	}
	override def vals = values.iterator
	override def assocs = (values zip values).iterator
	// here, set ops work directly on values:
	def contains(rhs: Val) = containsWeak(rhs)
	def containsWeak(rhs: Val) = values.exists(_ isEqualTo rhs)
	def containsStrong(rhs: Val) = values.exists(_ == rhs)

	def union(rhs: Val) = rhs match {
		case c: Container => Vector(values union c.vals.toSeq)
		case x => if (contains(x)) this else Vector(values ++ List(x))
	}

	def intersect(rhs: Val) = rhs match {
		case c: Container => Vector(values intersect c.vals.toSeq)
		case x => if (contains(x)) Vector(List(x)) else Vector.Empty
	}

	def diff(rhs: Val) = rhs match {
		case c: Container => Vector(values diff c.vals.toSeq)
		case x => Vector(values.filterNot(x.isEqualTo(_)))
	}


	override def addLeft(v: Val) = Vector( List(v) ++ values )
	override def addRight(v: Val) = Vector( values ++ List(v) )
	override def foreach(v: Val => Unit) = values.foreach(v)
	override def map(mapf: Val => Val) = Vector(values.map(mapf))
	override def forall(pred: Val => Boolean) = values.forall(pred)
	override def deepCmp(rhs: Val): Boolean = rhs match{
		case Vector(rs) => {
				for ((i,j) <- values.zip(rs))
					if (!(i isEqualTo j)) return false
				true
			}
		case _ => false
	}


    override def toString = {
		var result = "Vector("
		values match {
			case x :: xs => { result += x; xs foreach(result += ", " + _)}
			case _ =>
		}
		result += ")"
		result
	}
	override def toStr: String = values.map(_.coerce.toStr).mkString(
		"(", ", ", ")")

	override def toJava = {
		val result = new Array[AnyRef](values.size)
		for (i <- 0 until values.size){
			result(i) = values(i).toJava
		}
		result
	}

	def reduce(accu: Val)(f: (Val, Val) => Val): Val =
		(accu.coerce /: values.map(_.coerce))(f)
	
	override def convertTo(destType: Class[_]) = {
		if (!destType.isArray ) throw new EvalError(
			"Cannot convert Vector into non-array type '" +
			destType.getName + "´.")
		val dt = destType.getComponentType
		if (dt.isAssignableFrom(classOf[AnyRef])) toJava
		else try {
			Array.tabulate(values.size){n => (values(n) convertToPrimitive dt)} 
		} catch {
			case _: Throwable => throw new EvalError(
				"Cannot convert Vector into array of type '" +
				dt.getName + "´.")
		}
	}
	
	override def fits(destType: Class[_]) = if(!destType.isArray) 0 
	else {
		val dt = destType.getComponentType
		if (dt.isAssignableFrom(classOf[AnyRef])) 10 
		else dt match {
			case java.lang.Double.TYPE		=> 8
			case java.lang.Long.TYPE		=> 7
			case java.lang.Float.TYPE		=> 6
			case java.lang.Integer.TYPE		=> 5
			case java.lang.Short.TYPE		=> 4
			case java.lang.Byte.TYPE		=> 3
			case java.lang.Boolean.TYPE		=> 2
			case java.lang.Character.TYPE	=> 1
			case _ => 0 
		}
	}
}

private[see] object Vector {

	private val jarr = new Array[AnyRef](0)

	// comes to void as near as we may get
	object Empty extends Vector(Nil) {
		override def toString = "Empty"

		override def toBool: Boolean = false
		override def toLong: Long = 0
		override def size: Int = 0
		override def toJava = jarr
		override def isFlat = true
		override def map(mapf: Val => Val) = this
		override def reduce(accu: Val)(f: (Val, Val) => Val) = accu
	}

//	// Allow Construction from Val sequence
//	def apply(vs: Val*): Vector = Vector(vs)

	// converts java array into vector
	def fromJava(a : Array[AnyRef]): Vector = {
		if (a eq jarr) Empty else {
			val l = for (e <- a) yield Val(e)
			Vector(l.toList)
		}
	}
}
