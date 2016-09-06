/*
 * Nodes.scala
 * Defines elementary node types
 *
 */

package see.nodes

import see._
import see.values._
import see.Scope.CONSTNAME


/** A node that may occur on the left hand side of an assignment. */
private [see] trait LvNode extends Node{
	def setIn(s: Scope, value: Val)
}

// A node containing a literal value or the result of some simplification.
private[see] case class Constant(val v: Val) extends Final {
	assert(v.isInstanceOf[Value] || v.isInstanceOf[SymVal])

    def evalIn(s: Scope) = v
    override def isDefinedIn(s: Scope) = true
}

// A node containing a variable reference
private[see] case class Variable(val name: String) extends Final with LvNode {

	override def toString = "Variable(" + name+")"
	final def isConst = CONSTNAME matches name

	
    override def evalIn(s: Scope) = (s getVar name).getOrElse {
			builtins(name) match {
				case None => throw new Unresolved(name)
				case Some(op: Unary) => new BuiltinFunc(s, op)
			}
		}
		
	override def setIn(s: Scope, value: Val) = s.iset(name, value)

	// the simplest way to to this is to just try out...
    override def isDefinedIn(s: Scope) = !(s getVar name).isEmpty

	// If we have a well defined constant, use it.
	override def simplifyIn(s: Scope): Node = {
		(s getConst name) match {
			case Some(v: Value) => if (isConst) return Constant(v)
			case _ => if (s.isStableCheck) throw new Unresolved("const " + name)
		}
		this	// in all other cases: stay with the variable
	}
}

// A node that holds a list of other nodes which should be evaluated in parallel.
// Vector nodes may be nested.
private[see] case class Vnode(val nodes: Seq[Node]) extends Final {
	override def evalIn(s: Scope) =
		Vector(for(node <- nodes) yield node evalIn s)

	override def simplifyIn(s: Scope): Node = {
		if (nodes.size == 0)
			return Constant(Vector.Empty)

		val newNodes: Seq[Node] = for(node <- nodes) yield node.simplifyIn(s);
		if (newNodes.size == 1)
			newNodes.head // reduce to scalar
		else try {
			Constant(Vector(for(node <- newNodes) yield {
						node match {
							case Constant(v) => v.coerce
							case _ => throw new RuntimeException("")
						}
					}))
		} catch {
			case _: Throwable => Vnode(newNodes)
		}
	}

	override def isDefinedIn(s: Scope) = nodes.forall(_ isDefinedIn s)
	override def toString = nodes.mkString("Vnode(", ",", ")")
	override def isEmpty = nodes.isEmpty

	def isParamList = nodes.forall(_.isInstanceOf[Variable])
	def asArgs: Option[Seq[Variable]] = if (isParamList)
		Some(nodes.asInstanceOf[Seq[Variable]])
	else None
}

// A node that holds a list of other nodes which should be evaluated sequentially.
// Sequence nodes could be nested, but that doesn't have any sematic meaning.
// Blocks are created instead.
private[see] case class Nodes(val nodes: Seq[Node]) extends Final {

	override def evalIn(s: Scope) = {
		if (nodes.isEmpty) throw new EvalError("Empty expression")
		var r: Val = null
		for(node <- nodes) {
			//				println("Before: "+  s)
			r = s.exec(node)
			//				println("After: " +  s)
		}
		r
	}

	override def simplifyIn(s: Scope) = {
		if (nodes.isEmpty) EmptyNode
		else if (nodes.size == 1) nodes.head.simplifyIn(s)
		else Nodes{ for(node <- nodes) yield node.simplifyIn(s)}
	}

	override def isDefinedIn(s: Scope) = nodes.forall(_ isDefinedIn s)
	override def toString = nodes.mkString("Nodes(", ",", ")")
	override def isEmpty = nodes.isEmpty
}

// A sequence containing no nodes.
// Not quite the same as EmptyNode, but similar
private[see] object EmptyNodes extends Nodes(Nil) {
	override def isDefinedIn(s: Scope) = false
	override def evalIn(s: Scope) = throw new EvalError("Empty expression")
	override def simplifyIn(s: Scope) = this
}
