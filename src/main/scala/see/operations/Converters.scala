/*
 *  
 */

package see.operations

import math._

import see.Binary
import see.ParamError
import see.Scope
import see.Unary
import see.values._

// returns the Sel type of the variable as string
private[see] object VarType extends Unary("type") {

	override def apply(s: Scope, v: Val) = SymVal(v.selType)
}

// Checks, if var is of given type
private[see] object TypeCheck extends Binary("istype") {

	override def apply(lhs: Val, rhs: Val): Val = Bool{
		rhs match {
			case tn: Str => lhs.isType(Symbol(tn.toStr))
			case sn: SymVal => lhs.isType(sn.sym)
			case _ => throw new ParamError("Invalid type name: " + rhs.toStr)
		}
	}
}


private[see] abstract class Converter(op: String) extends Unary(op){

	// tries to parse string to get desired result
	override def apply(s: Scope, v: Val): Val = v match {
		case Str(v) => s.parseLocal(v) match {
				case None => throw new ParamError("Cannot parse: '" + v + "'.")
				case Some(n) => apply(s, n evalIn s)
			}
		case _ => super.apply(s, v)
	}
	
}

private[see] object Abs extends Unary("abs") {
	override def apply(s: Scope, v: Val): Val = v match {
		case n: Number => n.abs
		case _ => super.apply(s, v)
	}
}

private[see] object Round extends Unary("round") {
	override def apply(s: Scope, v: Val): Val = v match {
		case i: IntLike => v
		case Real(x) => Lint(round(x))
		case BigR(x) => BigI(x.round(BigR.MC_ROUND).toBigInt)
		case _ => super.apply(s, v)
	}
}

private[see] object Ceil extends Unary("ceil") {
	override def apply(s: Scope, v: Val): Val = v match {
		case i: IntLike => v
		case Real(x) => Lint(ceil(x).toLong)
		case BigR(x) => BigI(x.round(BigR.MC_CEIL).toBigInt)
		case _ => super.apply(s, v)
	}
}

private[see] object Floor extends Unary("floor") {
	override def apply(s: Scope, v: Val): Val = v match {
		case i: IntLike => v
		case Real(x) => Lint(floor(x).toLong)
		case BigR(x) => BigI(x.round(BigR.MC_FLOOR).toBigInt)
		case _ => super.apply(s, v)
	}
}


// Forced onversion to integer
private[see] object ToInt extends Converter("int") {
	override def apply(s: Scope, v: Val): Val = v match {
		case n: Number => Lint(n.toDouble.floor.toLong)
		case _ => super.apply(s, v)
	}
}

// Forced conversion to float
private[see] object ToReal extends Converter("real") {
	override def apply(s: Scope, v: Val): Val = v match {
		case n: Number => Real(n.toDouble)
		case _ => super.apply(s, v)
	}
}

// Forced conversion to BigR
private[see] object ToBig extends Converter("bigreal") {
	override def apply(s: Scope, v: Val): Val = v match {
		case n: Number => BigR(n.toBig)
		case _ => super.apply(s, v)
	}
}

// Forced conversion to BigI
private[see] object ToBigI extends Converter("bigint") {
	override def apply(s: Scope, v: Val): Val = v match {
		case n: Number => BigI(n.toBigI)
		case _ => super.apply(s, v)
	}
}

private[see] object ToBool extends Converter("bool") {

	override def apply(s: Scope, v: Val): Val = v match {
		case b: Bool => v
		case n: Number => Bool(n.toBool)
		case s: Str => Bool(v.toBool) // ! no try to reparse this!
		case _ => super.apply(s, v)
	}

}

// String conversion is always possible anyway
private[see] object ToStr extends Unary("str") {
	// coerce necessary here, because we don't call super.apply
	override def apply(s: Scope, v: Val) = Str(v.coerce.toStr)
}

// Same goes for regexes
private[see] object ToRexp extends Unary("regex") {
	// coerce necessary here, because we don't call super.apply
	override def apply(s: Scope, v: Val) = new Rexp(v.coerce.toStr)
}

// Forces argument into Vector
private[see] object ToVect extends Unary("vector") {
	// No collapse here! we want to keep any closures
	override def apply(s: Scope, v: Val) = Vector(v :: Nil)
}

// Forces argument into Table
private[see] object ToTable extends Unary("table") {
	// No collapse here! we want to keep any closures
	override def apply(s: Scope, v: Val) = Table(v)
}
// Forces argument into Map
private[see] object ToMap extends Unary("map") {
	// No collapse here! we want to keep any closures
	override def apply(s: Scope, v: Val) = ValMap(v)
}

// Retrieves contained values of map/assoc/table as vector
// Returns anything else unchanged
private[see] object GetValues extends Unary("values") {
	override def apply(s: Scope, v: Val) = v match {
		case ValMap(map) => Vector(map.values.toSeq)
		case Table(points) => Vector(for (p <-points) yield p.y)
		case Assoc(_, value) => value
		case _ => v
	}
}

// Retrieves keys of map/assoc/table as vector
// Returns anything else unchanged
private[see] object GetKeys extends Unary("keys") {
	override def apply(s: Scope, v: Val) = v match {
		case ValMap(map) => Vector(map.keys.toSeq)
		case Table(points) => Vector(for (p <-points) yield p.x)
		case Assoc(key, _) => key
		case _ => v
	}
}

// Merges two vectors into one vector of associations.
private[see] object ToAssoc extends Unary("assoc") {

	def assoc(key: Val, value: Val) = key match {
		case Vector(ks) => Vector(
				value match {
					case Vector(vs) =>
						for ((k,v) <- ks zip vs) yield Assoc(k,v)
					case v =>
						for (k <- ks) yield Assoc(k,v)
				}
			)
		case _ => Assoc(key, value)
	}

	override def apply(s: Scope, v: Val) = v match {
		case _: ValMap | _: Table => v // leave as is
		case Vector(Seq(l,r)) => assoc(l,r)
		case a: Assoc => a
		case other => Assoc(other,other)
	}
}
