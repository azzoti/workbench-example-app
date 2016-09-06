/*
 *  Branches.scala
 *  Defines nodes that form some kind of binary operation or
 *  other combinaton of two nodes.
 */

package see.nodes

import see.Binary
import see.EvalError
import see.Relator
import see.Scope
import see.operations.BoolAnd
import see.operations.TypeCheck
import see.values.Assoc
import see.values.Val


private[see] object Operation {
	def apply(op: Binary, pr: Int) = new Factory{
		// Inserts the left node during parsing, forming a final node
		override def apply(operand: Node) = new OperationP(op, operand, pr)
	}
}

private[see] class OperationP(val op: Binary, val rhs: Node, val prec: Int)
extends Proto {
	override def precedence = prec

	override def finish(n: Node): Option[Node] =
		Some(new Operation(op, n, rhs, prec))
}

// Node that defines an operation with two parameters.
// Represents typical infix operators.
private[see] class Operation(val operator: Binary, l: Node, r: Node, p: Int)
extends Branch(l, r, p)
{
	override def evalIn(s: Scope): Val = {
		// both operands are evaluated to the full!
		// Binary operators are not expected to work on anonyms, except:
		if (operator eq TypeCheck) TypeCheck(lhs evalIn s, rhs evalIn s)
		else {
			val lhsVal = s.coerce(lhs)
			if (operator.needsRhs(lhsVal)) operator(lhsVal, s.coerce(rhs))
			else operator(lhsVal, lhsVal) // don't eval rhs!
		}
	}

	override def simplifyIn(s: Scope): Node = {
		val ls = lhs.simplifyIn(s)
		val rs = rhs.simplifyIn(s)
		val simplified = new Operation(operator, ls, rs, prec)
		if (ls.isInstanceOf[Constant] && rs.isInstanceOf[Constant]) {
			try {
				// This should be reasonably safe, because assignments won't
				// work on constants and anything else should be fine.
				return new Constant(s checkStability simplified)
			} catch {
				case x: Throwable => if (s.isStableCheck) throw x
					// otherwise fallback to simplified
			}
		}
		simplified
	}

	override def isDefinedIn (s: Scope): Boolean = 
		operator.isDefinedFor(s, lhs, rhs)
	

	override def toString() = "(" + 
	(if(lhs != null) lhs.toString else "!NULL!") +
	" " + operator.opCode +" " +
	(if(rhs != null) rhs.toString else "!NULL!") + ")"
}


private[see] object Relation {

	def apply(op: Relator) = new Factory{
		override def apply(operand: Node) = new RelationP(op, operand)
	}
}
private[see] class RelationP(op: Relator, rhs: Node)
extends Proto {
	override def precedence = PREC.Relation

	override def finish(n: Node) = {
		if (n.isInstanceOf[Relation]) {
			val rel = new Relation(op, n.asInstanceOf[Relation].rhs, rhs)
			Some(new Operation(BoolAnd, n, rel, PREC.BoolAnd))
		}
		else Some(new Relation(op, n, rhs))
	}
}

// Specialization for relation operators, combination is different here
private[see] class Relation(op: Relator, l: Node, r: Node)
extends Operation(op, l, r, PREC.Relation)


// Association of two nodes
private[see] object Association extends Factory {
	override def apply(operand: Node) = new AssocP(operand)
}
private[see] class AssocP(val rhs: Node)
extends Proto {
	override def precedence = PREC.Assoc

	override def finish(n: Node): Option[Node] = Some(new Association(n, rhs))
}

private[see] class Association(key: Node, value: Node)
extends Branch(key, value, PREC.Assoc) {
	override def evalIn(s: Scope) = Assoc(lhs.evalIn(s).coerce, rhs.evalIn(s))

	override def simplifyIn(s: Scope): Node ={
		val skey = lhs.simplifyIn(s)
		val sval = rhs.simplifyIn(s)
		try {
			(skey, sval) match {
				case (Constant(k), Constant(v)) =>
					return Constant(Assoc(k.coerce, v))
			}
		} catch {
			case _: Throwable =>
		}
		new Association(skey, sval)
	}

	override def isDefinedIn(s: Scope) =
		(lhs isDefinedIn s) && (rhs isDefinedIn s)
	override def toString = "Association(" + lhs + " -> " + rhs + ")"
}


private[see] abstract class Assignment(lv: LvNode, r: Node)
extends Branch(lv, r, PREC.Assign) {
	def lv = lhs.asInstanceOf[LvNode]
}

// Node that performs direct assignment
private[see] object Assign extends Factory {
	override def apply(operand: Node) = new AssignP(operand)
}
private[see] class AssignP(rhs: Node) extends Proto {
	override def precedence = PREC.Assign

	// We may only assign to variables (chaining is allowed)
	override def finish(n: Node) = n match {
		case asg: Assignment => asg.rhs match {
				case lv: LvNode =>	
					asg.rhs = new Assign(lv, rhs)
					Some(asg)
				case _ => throw new EvalError(
						asg.rhs.toString + " is not an lvalue.")
			}
		case lv: LvNode => Some(new Assign(lv, rhs))
		case _ => throw new EvalError(n.toString + " is not an lvalue.")
	}
}

private[see] class Assign(l: LvNode, r: Node)
extends Assignment(l, r)
{
	override def evalIn(s: Scope): Val = { 
		val rval = (rhs evalIn s)
		lv.setIn(s, rval)
		rval 
	}

	override def simplifyIn(s: Scope) = lhs.simplifyIn(s) match {
		// note that lhs simplification is required for stability check!
		case lvn: LvNode => new Assign(lvn, rhs.simplifyIn(s))
		case other => throw new EvalError("Cannot assign to " + other.toString)
	}

	override def toString() = "(" + lhs + " = " + rhs + ")"
}


// Node that performs reassignment with some operation.
private[see] object ReAssign {
	def apply(proc: Binary) = new Factory{
		override def apply(operand: Node) = new ReAssignP(proc, operand)
	}
}

private[see] class ReAssignP(proc: Binary, rhs: Node) extends Proto {
	override def precedence = PREC.Assign

	// We may only assign to variables (chaining is allowed)
	override def finish(n: Node) = n match {
		case asg: Assignment => asg.rhs match {
				case lv: LvNode =>	
					asg.rhs = new ReAssign(proc, lv, rhs)
					Some(asg)
				case x => throw new EvalError(x.toString + " is not an lvalue.")
			}
		case lv: LvNode => Some(new ReAssign(proc, lv, rhs))
		case _ => throw new EvalError(n.toString + " is not an lvalue.")
	}
}

private[see] class ReAssign(val proc: Binary, l: LvNode, r: Node)
extends Assignment(l, r)
{
	override def evalIn(s: Scope): Val = {
		val result = proc(s.coerce(lhs), s.coerce(rhs))
		lv.setIn(s, result)
		result
	}
	
	override def simplifyIn(s: Scope) =  lhs.simplifyIn(s) match {
		case lvn: LvNode => new ReAssign(proc, lvn, rhs.simplifyIn(s))
		case other => throw new EvalError("Cannot assign to " + other.toString)
	}

	override def toString() = "(" + lhs + " " + proc.opCode + "= " + rhs + ")"

}


