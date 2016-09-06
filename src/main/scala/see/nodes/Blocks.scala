/*
 *  
 */

package see.nodes

import see.Scope
import see.ResultException
import see.OuterScope
import see.values._

// Provides an intermediate scope that catches accesses to wildcard parameters.
private class ArgInterceptor(val enclosing: Scope) {
	
	// This Parent is used to detect argument usage.
	private class Detector(delegate: Scope) extends OuterScope(delegate){

		override val stable = delegate.isStable
		override val stableCheck = delegate.isStableCheck

		override def contains(name: String) =
			if (isWc(name)) true else super.contains(name)

		override def set(name: String, v: Val) =
			if (isWc(name)) true else super.set(name, v)

		override def getVar(name: String) = 
			if (isWc(name)) Some(Vector.Empty) else super.getVar(name)
		
		override def getConst(name: String) = 
			if (isWc(name)) Some(Vector.Empty) else super.getConst(name)

		override def inner(s: Scope) = delegate.createInner

	}

	private val ARGP = "_(\\d)?".r
	private var argsUsed = -1;

	private def isWc(name: String): Boolean = {

		val m = ARGP.pattern.matcher(name)
		if (!m.matches) return false
		val n = m.group(1) match {
			case null => 0
			case x => x.toInt
		}

		if (argsUsed < n)
			argsUsed = n
		true
	}
	
	private var willBeStable = true;

	def simplify(n: Node): Node = {
		try{
			val det = Scope(new Detector(enclosing.createStableCheck))
			return n simplifyIn det
		} catch {
			case x: Throwable => {
					// println("Stable simplify failed: " + x)
					if (enclosing.isStableCheck) throw x // notify outer check
				}
		}
		willBeStable = false
		n simplifyIn Scope(new Detector(enclosing))
	}
	
	def isFunction = argsUsed >= 0

	def createFunction (code: Node): Node = {
		val paramList: Seq[Variable] = if (argsUsed == 0) List(Variable("_"))
		else for (n <- 1 to argsUsed) yield Variable("_" + n)

		if (willBeStable)
			new StableFnode(Fnode.newName, paramList, code)
		else
			Fnode(paramList, code)
	}

}

// Creates a local scope to execute content. Spans up an anonymous closure.
private[see] case class Block(content: Node) extends Leaf {

	// class embedded into closure:
	private class Eval extends Leaf{

		override def evalIn(s: Scope) = try {
			s.exec(content)
		} catch {
			case ResultException(r) => s.setResult(r.coerce)
		}
	}
	

	// if it's defined in outer scope, it will be in inner
	override def isDefinedIn(s: Scope) = content isDefinedIn s
	override def toString = "[Block[" + content + "]]" // better to see

	override def evalIn(s: Scope) = Anonym(s.createInner, new Eval)

	override def simplifyIn(s: Scope) = {
		val ai = new ArgInterceptor(s)
		val n = ai.simplify(content)
		if (ai.isFunction)	ai.createFunction(n)
		else n match {
			// Having just another block or constant as content is pointless:
			// Not so for variables!
			case _: Block | _: CatchBlock | _: Constant => n
			case _ => Block(n)
		}
	}
}


// A Block with an optional catch phrase.
// Any eval- or other exception thrown within such a block will be converted
// into an error that can be further evaluated.
// A CatchBlock is NOT a Block! Both must be cleanly distiguishable.
private[see] case class CatchBlock(content: Node, footer: Node) extends Leaf {

	// class embedded into closure:
	private class Catcher extends Leaf{
		override def evalIn(s: Scope) = {
			try{
				s.exec(content)
			} catch {
				case ResultException(r) => s.setResult(r.coerce)
				case ex: Exception => {
						s.setResult( Str(ex.toString)) // so it may be checked by footer
						if (footer.isEmpty) Vector.Empty // just swallow failure
						else footer evalIn s
					}
			}
		}
	}

	// if it's defined in outer scope, it will be in inner
	override def isDefinedIn(s: Scope) = {
		content.isDefinedIn(s) && footer.isDefinedIn(s)
	}
	override def toString = "[try[" + content + "]catch[" + footer + "]]"

	override def evalIn(s: Scope) = Anonym(s.createInner, new Catcher())

	override def simplifyIn(s: Scope) = {
		val ai = new ArgInterceptor(s)
		val n = ai.simplify(content)
		val sf = ai.simplify(footer)
		val result = CatchBlock(n match {
				// Having just another block as content is pointless:
				case b: Block => b.content
				case other => other
			}, sf)
		if (ai.isFunction) ai.createFunction(result)
		else result
	}
}


