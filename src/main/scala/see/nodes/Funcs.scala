/*
 *  
 */

package see.nodes

import see.EvalError
import see.ParamError
import see.Scope
import see.StableScope
import see.values.UserFunc
import see.values.Val
import see.values.Vector

private[see] object Fnode {
	private var instanceCount = 0
	private def next = { instanceCount += 1; instanceCount }

	def newName = "<afun_" + next + ">"

	def apply(params: Seq[Variable], code: Node) =
		new Fnode(newName, params, code)
}


private[see] class FnodeP(code: Node) extends Proto {
	override def precedence = PREC.Anondef

	// We need a varaible or vector of variables as insertion point.
	// It will be replaced by the resulting Fnode.
	// Note that Fnodes created from plain blocks won't use this.
	override def finish(n: Node): Option[Fnode] = {
		val args = n match {
			case v: Variable => Some(List(v))
			case vn: Vnode if vn.isParamList => vn.asArgs
			case _ => return None // No suitable parameter list
		}
		Some(Fnode(args.get, code))
	}
}

// A node that contains a function definition for an anonymous function.
private[see] class Fnode(
	val name: String, val params: Seq[Variable], val code: Node)
extends Leaf {
	override def dump = toString + "\nDefinition: " + code.dump

    override def evalIn(s: Scope) = new UserFunc(s, this)

	override def simplifyIn(s: Scope) = try {
		// If this works, we have a free function that can be used right away
		// Note that it still may refer to global consts, if they are already defined.
		val cs = s.createStableCheck
		for (p <- params) cs.setLocal(p.name, Vector.Empty)
		val simpleCode = if (code.isInstanceOf[Block])
			code.asInstanceOf[Block].content.simplifyIn(cs)
		else code.simplifyIn(cs)
		new StableFnode(name, params, simpleCode)
	} catch {
		// Otherwise try full closure (we don't need to define parameters in advance)
		case _: Throwable => {
				val sc = if (code.isInstanceOf[Block])
					code.asInstanceOf[Block].content.simplifyIn(s)
				else code.simplifyIn(s)
				new Fnode(name, params, sc)
			}
	}

    override def isDefinedIn(s: Scope) = {
		val cs = s.createInner
		for (p <- params) cs.setLocal(p.name, Vector.Empty)
		code.isDefinedIn(cs)
	}

    override def toString = "Function " + name +
	params.map(_.name).mkString("(", ",", ")") + " := " + code;

	private def paramFail(got: Int) = new ParamError(
		"Function " + name + " requires " + params.size +
		" parameters, got " + got + ".")

	// Call during evaluation
	def call(defScope: Scope, args: Val): Val = {
		val inner = defScope.createInner
		args match {
			case Vector(vs) => {
					// if just one parameter, use the whole vector for that
					if (params.size == 1) inner.setLocal( params.head.name, args)
					else if (vs.size != params.size) throw paramFail(vs.size)
					else for ((p, v) <- params zip vs) inner.setLocal(p.name, v)
				}

			case x => if (params.size != 1) throw paramFail(1)
				else inner.setLocal( params.head.name, args)
		}
		code evalIn inner
	}
	val stable = false
}


// A node that contains a function definition.
private[see] class StableFnode(n: String, p: Seq[Variable], c: Node)
extends Fnode(n,p,c)
{
	override def evalIn(s: Scope) = new UserFunc(
		new Scope(s.parser, StableScope), this)
	override def toString = "Stable " + super.toString
    override def isDefinedIn(s: Scope) = true // or else we would not have this
	override val stable = true
}

private[see] class FundefP(code: Node) extends Proto {
	override def precedence = PREC.Fundef

	// We need a Fcall as insertion point.
	// It will be replaced by the Fundef.
	override def finish(n: Node): Option[Fundef] = {
		// check, whether insertion poínt is suitable:
		if (!n.isInstanceOf[Fcall]) return None  // No valid function header
		val fc = n.asInstanceOf[Fcall]
		val argList = fc.argList
		if (argList.isParamList) // Invalid argument list:" + argList)
			Some(new Fundef(fc.fname, argList.asArgs.get, code))
		else None
	}
}

// A node that contains a function definition (will be defined when evaluated).
private[see] class Fundef(n: String, ps: Seq[Variable], c: Node)
extends Fnode(n, ps, c) {
	def this(f: Fnode) = this(f.name, f.params, f.code)

	// evaluating this node means to set a variable with the function's name.
    override def evalIn(s: Scope) = {
		val f = new UserFunc(s, this)
		s.iset(name, f)
		f
	}

	override def simplifyIn(s: Scope) = {
		super.simplifyIn(s) match {
			case f: StableFnode => new StableDef(f)
			case f: Fnode => new Fundef(f)
			case _ => throw new EvalError("Strange simplify result.")
		}
	}
}

// A node that contains a function definition.
private[see] class StableDef(n: String, p: Seq[Variable], c: Node)
extends Fundef(n,p,c)
{
	def this(f: StableFnode) = this(f.name, f.params, f.code)

	override def evalIn(s: Scope) = {
		val f = new UserFunc(new Scope(s.parser, StableScope), this)
		s.iset(name, f)
		f
	}
    override def isDefinedIn(s: Scope) = true // or else we would not have this
	override def toString = "Stable " + super.toString
	override val stable = true
}

