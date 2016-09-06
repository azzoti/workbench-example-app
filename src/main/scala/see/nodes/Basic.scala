/*
 *  
 */

package see.nodes

import see.EvalError
import see.INode
import see.Scope

// A prototype of a Node.
// It will be converted ito a real node when it is inserted into the tree.
private[see] trait Proto {

	def precedence: Int

	// returns none, if it won't accept n as insertion point
	def finish(n: Node): Option[Node]
}

// A factory to create nodes while parsing.
private [see] trait Factory{
	def apply(operand: Node): Proto
}
private [see] trait Factory2 extends Factory{
	def apply(operand: Node): Proto = EmptyNode
	// we expect only this one to be used!
	def apply(op1: Node, op2: Node): Proto
}

private[see] trait Node extends INode {

	override def simplifyIn(s: Scope) = this
    override def isDefinedIn(s: Scope) = false
	def isEmpty = false

	def precedence: Int
	def find(precedence: Int): Option[Node] = None
	def insert(parent: Branch, newNode: Proto): Boolean

	def dump = toString
}


// A Leaf is a node that terminates a branch within an expression tree.
private[see] trait Leaf extends Node {

	def precedence = 0

	final def insert(parent: Branch, proto: Proto) = {
		// assert tree is always full after insert
		assert (parent.lhs != null) // node must be final, once inserted
//		if (parent.lhs == null) {
//			assert(proto.isInstanceOf[Leaf])
//			// parent.lhs = newNode.finish()
//			false
//		}
//		else {
		assert(parent.rhs eq this)
		proto.finish(this) match {
			case None => false
			case Some(n) => parent.rhs = n; true
//			}
		}
	}
}

private[see] trait Final extends Leaf with Proto {

	def finish(n: Node): Option[Node] = Some(this)
}


// Node that contains nothing.
// Only used during parsing. Should never be evaluated.
private[see] case object EmptyNode extends Final {

	def evalIn(ev: Scope) = throw new EvalError("Empty Node")
	
	override def isEmpty = true
}


private[see] abstract class Branch(val lhs: Node, var rhs: Node, val prec: Int)
extends Node 
{
	override def precedence = prec

	// Looks for insertion point
	override def find(destPrecedence: Int): Option[Node] = {
		if (prec < destPrecedence) None // further nodes will have lower preceence
		else if (prec == destPrecedence) Some(this)
		else rhs.find(prec)
	}

	override final def insert(parent: Branch, proto: Proto) = {
		assert (parent.rhs eq this)
		assert (lhs != null) // must have been finished already
		if (proto.precedence >= prec)
			proto.finish(this) match {
				case Some(n) => parent.rhs = n; true
				case None => false
			}
		else rhs.insert(this, proto)
	}
}


// A Leaf that takes op the current rightmost operand forming a new leaf.
// It is used as superclass for atomic operations.
private[see] abstract class Atom(val opd: Node) extends Leaf
