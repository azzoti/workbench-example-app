/*
 * Nodes that affect control flow.
 */

package see.nodes

import see.EvalError
import see.ResultException
import see.Scope
import see.SeeException
import see.values.Bool
import see.values.Val



// Node for the conditional operator. This takes three parameters (p1 ? p2 : p3)
private[see] object Condition extends Factory2 {
	override def apply(tc: Node, fc: Node) = new ConditionP(tc, fc)
}

private[see] class ConditionP(val ifTrue: Node, val ifFalse: Node)
extends Proto {
	override def precedence = PREC.Condition
	override def finish(n: Node) = Some(new Condition(n, ifTrue, ifFalse))
}

private[see] class Condition(c: Node, val ifTrue: Node, val ifFalse: Node)
extends Atom(c) {

	override def evalIn(s: Scope): Val = {
		val condVal = s.exec(opd)
		if (condVal.toBool)
			ifTrue evalIn s
		else
			ifFalse evalIn s
	}

	override def simplifyIn(s: Scope) = {
		val c = opd.simplifyIn(s)
		c match {
			case Constant(v) => if (v.toBool) ifTrue.simplifyIn(s)
				else ifFalse.simplifyIn(s)

			case _ => new Condition(
					c, ifTrue.simplifyIn(s), ifFalse.simplifyIn(s) )
		}
	}

    override def isDefinedIn(s: Scope) = {
		if (opd isDefinedIn s) {
			if ((opd evalIn s).toBool) ifTrue isDefinedIn s
			else ifFalse isDefinedIn s
		}
		else false
	}

	override def toString() = "[" + opd + " ? " + ifTrue + " : " + ifFalse + "]"
}

// Node for the loop operator.
// The loop body will be executed until condition is false.
// If it never evaluates to true, the def node will be evaluated exactly once
// and its result is returned.
// This takes three parameters (p1 ?? p2 : p3)
private[see] object Loop extends Factory2 {
	override def apply(body: Node, dflt: Node) = new LoopP(body, dflt)
}

private[see] class LoopP(val body: Node, val dflt: Node) extends Proto {
	override def precedence = PREC.Condition
	override def finish(n: Node) = Some(new Loop(n, body, dflt))
}

private[see] class Loop(c: Node, val body: Node, val dflt: Node)
extends Atom(c) {

	override def evalIn(s: Scope): Val = {
		while (s.exec(opd).toBool) {
			val result = s.exec(body) // required here to ensure execution
			if(!s.exec(opd).toBool)
				return result
		}
		dflt evalIn s
	}

	override def simplifyIn(s: Scope) = {
		val c = opd.simplifyIn(s)
		c match {
			case Constant(Bool(false)) => dflt.simplifyIn(s)
			case Constant(Bool(true)) =>
				throw new SeeException("Infinite loop detected.")
			case _ => new Loop(
					c, body.simplifyIn(s), dflt.simplifyIn(s))
		}
	}

    override def isDefinedIn(s: Scope) = {
		if (opd isDefinedIn s) {
			if ((opd evalIn s).toBool) body isDefinedIn s
			else dflt isDefinedIn s
		}
		else false
	}

	override def toString() = "[" + opd + " ?? " + body + " : " + dflt + "]"
}


// Asserts a certain condition. Throws eval error if condition fails.
private[see] object Assertion extends Factory {
	override def apply(res: Node) = new AssertionP(res)
}
private[see] class AssertionP(val res: Node) extends Proto {
	override def precedence = PREC.Assertion
	override def finish(n: Node) = Some(new Assertion(n, res))
}

private[see] class Assertion(c: Node, fail: Node)
extends Atom(c) {

	override def isDefinedIn(s: Scope) =
		opd.isDefinedIn(s) && fail.isDefinedIn(s)

	override def toString = "assert " + opd + " : " + fail

	override def evalIn(s: Scope) = {
		if (s.exec(opd).toBool) s.getResult
		else {
			val msg = fail.evalIn(s).toString
			throw new EvalError(msg)
		}
	}

	override def simplifyIn(s: Scope) =
		new Assertion(opd.simplifyIn(s), fail.simplifyIn(s))
}


// Terminates current block with result if condition is true.
// Roughly equivalent to if (cond) return result;
private[see] object Return extends Factory {
	override def apply(res: Node) = new ReturnP(res)
}
private[see] class ReturnP(val res: Node) extends Proto {
	override def precedence = PREC.Return
	override def finish(n: Node) =  Some(new Return(n, res))
}

private[see] class Return(c: Node, result: Node)
extends Atom(c) {

	override def isDefinedIn(s: Scope) =
		opd.isDefinedIn(s) && result.isDefinedIn(s)

	override def toString = "if (" + opd + ") return " + result

	override def evalIn(s: Scope) = {
		if (s.exec(opd).toBool)	throw ResultException(result evalIn s)
		else s.getResult
	}

	override def simplifyIn(s: Scope) =
		new Return(opd.simplifyIn(s), result.simplifyIn(s))
}



