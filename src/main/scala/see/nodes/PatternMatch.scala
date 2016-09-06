/*
 *
 */

package see.nodes

import see.EvalError
import see.Scope
import see.operations.Equal
import see.values._

private[see] class Alternative(val pattern: Any, val result: Node){

	private val sr = Some(result)
	private def onMatch(pred: Boolean) = if(pred) sr else None

	def take(sel: Val, s: Scope): Option[Node] = {

		def cmp(pv: Val) = Equal.apply(sel.coerce, pv.coerce).toBool

		pattern match {
			// default alternative
			case "?" => sr

				// type pattern
			case Constant(SymVal(sym)) => onMatch(sel isType sym)

				// regex pattern will be matched
			case Constant(rx: Rexp) => onMatch((rx.~(sel.coerce)).toBool)

				// other constants are compared verbatim
			case Constant(x) => onMatch(cmp(x))

			case Variable(sym) => s.getVar(sym) match {

					// functions will be called and the result checked for truth
					case Some(f: Functional) => onMatch(f.call(s, sel).coerce.toBool)

						// Note: It doesn't make sense to treat Anonyms special at this point,
						// because they won't be affected by the selector anymore.

						// other variables are also compared verbatim
					case Some(v: Val) => onMatch(cmp(v))
					case _ => None // try next alternative
				}

				// Any other nodes will be evaluated
				// and the result is checked for truth.
				// It is assumed that the resulting expression refers to the
				// selector via $, otherwise the result won't depend on
				// the selector at all
			case n: Node => onMatch( (n evalIn s).coerce.toBool)

			case _ => None
		}
	}

	override def toString() = ":" + pattern + " -> " + result + "; "
}


private[see] class MatcherP(alts: Seq[Alternative]) extends Proto {
	override def precedence = PREC.Matcher
	override def finish(n: Node) = Some(new Matcher(n, alts))
}

/** Node for pattern match.
 * This construct is obviously inspired by the scala pattern matching
 * mechanism, which is very powerful.
 * Although this matcher is far less versatile, it still provides useful
 * functionality.
 * And it is the only way to detect the type of an expression.
 */
private[see] class Matcher(sel: Node, alternatives: Seq[Alternative])
extends Atom(sel) {

	private def selectIn(sel: Val, s: Scope): Node = {
		// $ may be used within pattern,
		// restored each time, in case previous match has changed it
		for (a <- alternatives) {
			s.setResult(sel)
			for (r <- a.take(sel, s)) { return r}
		}
		
		// nothing found!
		throw new EvalError("No matching alternative for " + sel)
	}

	override def evalIn(s: Scope) = {
		val sel = s.exec(opd)
		val r = selectIn(sel, s)

		// ensure correct $ for result eval, in case it was changed during match
		s.setResult(sel)
		r evalIn s
	}

	override def simplifyIn(s: Scope) = try {
		val sel = opd.simplifyIn(s)
		// if s is const, we may replace the whole node with the match result,
		// otherwise:
		val newa = for (a <- alternatives) yield
			new Alternative(a.pattern match {
					case n: Node => n.simplifyIn(s)
					case x => x
				}, a.result.simplifyIn(s))
		new Matcher(sel, newa)
	} catch {
		case _: Throwable => this
	}
	
	override def isDefinedIn(s: Scope): Boolean = {
		if (!opd.isDefinedIn(s)) // pure variable name always regarded as defined, even if it isn't
			return false

		for (a <- alternatives) {
			if (!a.result.isDefinedIn(s))
				return false
			a.pattern match {
				case n: Node => if (!n.isDefinedIn(s)) return false
				case _ =>
			}
		}
		true
	}

	override def toString() = "(" + opd + " ?~ [" + alternatives + "])"
}
