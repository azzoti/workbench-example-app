/*
 *  
 */

package see.nodes

import see.EvalError
import see.Illegal
import see.Scope
import see.Unary
import see.Unknown
import see.Unresolved
import see.builtins
import see.operations.Symbolize
import see.values._
import see.Scope.CONSTNAME

private[see] class LocalP extends Proto{
	override def precedence = 0

	// requires parameter list
	override def finish(n: Node): Option[Local] = {
		val locals = n match {
			case v: Variable => List(v)
			case vn: Vnode if vn.isParamList => vn.asArgs.get
			case _ => return None // No suitable parameter list
		}
		Some(new Local(locals))
	}
}

// Node that declares its arguments as local variables,
// which are still undefined.
private[see] class Local(locals: Seq[Variable]) extends Leaf
{
	override def evalIn(s: Scope) = {
		for (v <- locals){
			s.setLocal(v.name, NullVal)
		}
		Bool.False
	}

	override def simplifyIn(s: Scope): Node = this

    override def isDefinedIn(s: Scope) = true
	override def toString() = "[local(" + locals + ")]"
}


private[see] class DefinedP extends Proto{
	override def precedence = 0
	override def finish(n: Node) = Some(new Defined(n))
}

// Node that checks, whether its operand is defined within current scope.
private[see] class Defined(o: Node) extends Atom(o)
{
	override def evalIn(s: Scope) = Bool(opd isDefinedIn s)

	override def simplifyIn(s: Scope): Node = {
		val simple = opd.simplifyIn(s)
		if (simple.isInstanceOf[Constant]) Constant(Bool(true))
		else new Defined(simple)
	}

    override def isDefinedIn(s: Scope) = true
	override def toString() = "[defined(" + opd + ")]"
}

// Node that defines operation with a single parameter, which may be a vector.
// In fact, all functions are represented as Prefix node with a vector argument.
private[see] class PrefixP(val operator: Unary) extends Proto {
	override def precedence = 0
	override def finish(n: Node) = operator match {
		case Symbolize => Some(new Indirection(n))
		case _ => Some(new Prefix(n, operator))
	}
}

private[see] class Prefix(o: Node, val operator: Unary)
extends Atom(o)
{
	override def evalIn(s: Scope): Val = {
		// evaluate operand
		val arg = opd evalIn s
		operator(s, arg)
	}

	override def simplifyIn(s: Scope): Node = {
		val simple = opd.simplifyIn(s)
		val simplified = new Prefix(simple, operator)
		if (simple.isInstanceOf[Constant])
			return new Constant(s checkStability simplified)
		simplified
	}
	
	override def isDefinedIn(s: Scope) = opd isDefinedIn s
	override def toString() = "["+ operator.opCode + "(" + opd + ")]"
}

private[see] class Indirection(o: Node)
extends Prefix(o, Symbolize) with LvNode
{
	override def evalIn(s: Scope) = operator(s, opd evalIn s)
	override def setIn(s: Scope, value: Val) {
			val iname = s.coerce(opd)
			s.iset(iname.toStr, value)
		}
	
	override def simplifyIn(s: Scope): Node = {
		if (s.isStableCheck) throw new EvalError("Indirect name is unstable")
		val simple = opd.simplifyIn(s)
		new Indirection(simple)
	}
	
	override def toString() = "[indirect(" + opd + ")]"
}

private[see] class Fcall(val fname: String, args: Node)
extends Atom(args)
{
	def argList = opd.asInstanceOf[Vnode]

	override def evalIn(s: Scope): Val = {
		// evaluate operand
		val arg = opd evalIn s
		// we try function variables first:
		(s getVar fname) match {
			case Some(func: Callable) => func.call(s, arg)
			case Some(x) => x.coerce match {
					case f: Callable => f.call(s, arg)
					case other => throw new Illegal(
							"Tried to call '" + fname + "'["+ other.getClass() +
							"], which is not callable.")
				}
			case None => builtins(fname) match {
					case None => throw new Unknown(fname)
					case Some(op: Unary) => op(s, arg)
				}
		}
	}

	override def simplifyIn(s: Scope): Node = {
		val simple = opd.simplifyIn(s)
		val simplified = new Fcall(fname, simple)

		if (fname == "rnd") {  // never simplify a random generator
			if (s.isStableCheck)
				throw new EvalError("rnd is unstable")
			else return simplified
		}

		// We cannot simplify user function calls in general,
		// because they may be redefined at any time.
		// So restrict to const patterns and built-ins.
		// In principle, built-ins wouldn't be allowed either,
		// but we keep them for performance reasons.
		// So if you insist on overwriting built-in functions,
		// at least do so early and within a separate expression or live
		// with the consequences.
		//
		if (simple.isInstanceOf[Constant]) {

			if (CONSTNAME.matches(fname) || builtins.contains(fname) ) try {
				return new Constant(s checkStability simplified)
			} catch {
				case ex: Throwable => if (s.isStableCheck) throw ex // fail ongoing check
			}
		}

		// for simplicity, we only allow built-ins to be eliminated
		if (s.isStableCheck && (s.contains(fname) || !builtins.contains(fname)))
			throw new Unresolved("Unstable function: " + fname)
		simplified
	}

	override def isDefinedIn(s: Scope) = {
		(opd isDefinedIn s) && ((s getVar fname) match {
				case Some(func: Callable) => func.isDefinedIn(s)
				case Some(x) => false
				case None => builtins.contains(fname)
			} )
	}

	override def toString() = "["+ fname + "(" + opd + ")]"
}
