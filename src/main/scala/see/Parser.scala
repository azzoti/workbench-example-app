/*
 * Parser.scala
 *
 */

package see

import java.io.BufferedReader
import java.io.Reader
import java.io.StringReader
import scala.util.parsing.combinator._
import see.parser.Expressions
import see.parser.Program


private trait Parser {

	def parse(r: Reader, scope: Scope): INode
	def parse(e: String, scope: Scope): INode =
		parse(new BufferedReader(new StringReader(e)), scope)
	def apply(e: String, scope: Scope) = parse(e, scope)

	private[see] def test(r: Reader): Unit
	private[see] def test(e: String): Unit =
		test(new BufferedReader(new StringReader(e)))

	/** Removes C-style comments from input.
	 *
	 * We strip comments before handing the input to the parser chain.
	 * This makes things much easier than doing that within the parser itself.
	 *
	 * Note that we won't support nested block comments.'
	 */
	protected def stripComments(r: Reader): Reader = {
		val sb = new StringBuilder()
		val sr = if(r.isInstanceOf[BufferedReader])
			r.asInstanceOf[BufferedReader]
		else
			new BufferedReader(r)
		var inComment = false
		var commentStart = 0
		var line = sr.readLine
		var lineCount = 1
		while (line != null){
			// process line comments first,
			var lcs = line.indexOf("//")
			if (lcs >= 0) line = line.substring(0, lcs)
			// then any remaining block comments
			if (inComment){
				lcs = line.indexOf("*/")
				line = if (lcs >= 0) {
					inComment = false
					line.substring(lcs+2)
				} else ""
			} else {
				line = line.replaceAll("/\\*.*?\\*/", "")
				lcs = line.indexOf("/*")
				if (lcs >= 0) {
					inComment = true
					commentStart = lineCount
					line = line.substring(0, lcs)
				}
			}
			sb.append(line) // even if empty,
			sb.append("\n") // so parser error will show correct position
			line = sr.readLine
			lineCount += 1
		}
		if (inComment)
			throw new ParseException("Unclosed Comment on line " + commentStart)
		new StringReader(sb.toString)
	}
}

/** Supports full Sel syntax, including functions, blocks etc.
 *
 */
private class SelParser extends Parser {

	private val ep = new Program()

	private[see] override def test(input: Reader) {
		val result = ep.parseAll(ep.all, stripComments(input))
		println ("" + result + "\n of type " + result.getClass)
		if (result.isInstanceOf[ep.Success[_]]) {
			val outcome = result.get
			println ("Embedded result(" + outcome.getClass + "): " + outcome)
		}
	}

	override def parse(input: Reader, scope: Scope): INode = {
		ep.parseAll(ep.all, stripComments(input)) match {
			case ok: ep.Success[_] => ok.get.simplifyIn(scope);
			case x => throw new ParseException(x.toString)
		}
	}
}

/** The const parser only works on expressions that will eventually
 * produce a constant result.
 * It does not know about variables or functions, which makes parsing quite
 * efficient and simple, but limits usage scenarios severely.
 *
 * Nevertheless it might be useful, as it allows more expressive input patterns
 * than pure numbers.
 * E.g. instead you may write 1/256 or 1/2**8 instead of the rather
 * mysterious 0.00390625.
 *
 */
private class ConstParser extends Parser {

	private val ep = new Expressions()

	private[see] override def test(input: Reader) {
		val result = ep.parseAll(ep.all, stripComments(input))
		println ("" + result + "\n of type " + result.getClass)
		if (result.isInstanceOf[ep.Success[_]]) {
			val outcome = result.get
			println ("Embedded result(" + outcome.getClass + "): " + outcome)
		}
	}

	override def parse(input: Reader, scope: Scope): INode = {
		ep.parseAll(ep.all, stripComments(input)) match {
			case ok: ep.Success[_] => ok.get.simplifyIn(scope);
			case x => throw new ParseException(x.toString)
		}
	}
}

