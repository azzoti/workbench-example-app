/*
 *  
 */

package see.parser

import java.lang.{Long => JLong }
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import see.ParseException
import see.nodes._
import see.values._


private object Literals {

	val TRUE = "[Tt][Rr][Uu][Ee]".r
	val FALSE = "[Ff][Aa][Ll][Ss][Ee]".r
	val E = "E"
	val PI = "PI"

	val BINARY = "0b[01][01_]*".r // matches binary literals
	val HEX = "(0x[0-9a-fA-F][0-9a-fA-F_]*L?)".r
	val RBINARY = "[01][01_]*".r // matches binary literals
	val RHEX = "[0-9a-fA-F][0-9a-fA-F_]*L?".r
	// Assembly-style suffix dropped for speed. Incompatible with L anyway
	//	val HEX = "(0x[0-9a-fA-F][0-9a-fA-F_]*L?)|([0-9a-fA-F][0-9a-fA-F_]*h)".r

	// note: No sign here! will be handled by operand parsing!
	val DEC = """\d+L?""".r
	val REAL = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?L?""".r

	val REXP = ("'"+"""(\\'|[^'])*"""+"'").r
	val STRING = ("\""+"""([^"\p{Cntrl}\\]|\\[^u\p{Cntrl}]|\\u[a-fA-F0-9]{4})*"""+"\"").r

	val NAME = "[a-zA-Z$#_][a-zA-Z0-9$#._]*".r
	val OPERATOR = "[-!%&*+/:<=>?@^~|]+".r

	// patterns may be used from within Sel as e.g. REGEX_HEX
	val RxPatterns = Map("DECIMAL" -> Constant(Rexp(DEC.pattern)),
						 "FLOAT"   -> Constant(Rexp(REAL.pattern)),
						 "BINARY"  -> Constant(Rexp(BINARY.pattern)),
						 "HEX"     -> Constant(Rexp(HEX.pattern)),
						 "STRING"  -> Constant(Rexp(STRING.pattern)),
						 "REGEX"   -> Constant(Rexp(REXP.pattern)),
						 "NAME"    -> Constant(Rexp(NAME.pattern)),
						 "TRUE"    -> Constant(Rexp(TRUE.pattern)),
						 "FALSE"   -> Constant(Rexp(FALSE.pattern))
	)

	val TYPENAMES = Map("Integral"	-> Constant(SymVal('Integral)),
						"Int"		-> Constant(SymVal('Int)),
						"Real"		-> Constant(SymVal('Real)),
						"String"	-> Constant(SymVal('String)),
						"Bool"		-> Constant(SymVal('Bool)),
						"BigInt"	-> Constant(SymVal('BigInt)),
						"BigReal"	-> Constant(SymVal('BigReal)),
						"Regex"		-> Constant(SymVal('Regex)),
						"Function"	-> Constant(SymVal('Function)),
						"Anonymous" -> Constant(SymVal('Anonymous)),
						"Closure"	-> Constant(SymVal('Closure)),
						"Native"	-> Constant(SymVal('Native)),
						"Number"	-> Constant(SymVal('Number)),
						"Comparable" -> Constant(SymVal('Comparable)),
						"Scalar"	-> Constant(SymVal('Scalar)),
						"Value"		-> Constant(SymVal('Value)),
						"Any"		-> Constant(SymVal('Any)),
						"Symbol"	-> Constant(SymVal('Symbol)),
						"Container"	-> Constant(SymVal('Container)),
						"Assoc"		-> Constant(SymVal('Assoc)),
						"Table"		-> Constant(SymVal('Table)),
						"Map"		-> Constant(SymVal('Map)),
						"Vector"	-> Constant(SymVal('Vector)),

						"Null"		-> Constant(SymVal('Null))
	)

}

// Parses literals, superclass for other parsers.
private[see] trait Literals extends RegexParsers {

	import Literals._

	final def constant: Parser[Constant] = cString | cRexp | numeric

	final def numeric = ("0b" ~> cBin) | ("0x" ~> cHex) | cDecimal // cFloat ||| cDec
	// ||| required,
	// otherwise we would always get floats, since decimals are a subset
	// Checking base prefix here speeds things up a bit.

	// ==========================================================
	// Constant literals

	final def cString = STRING ^^ {
		// s still contains quotes!
		s => Constant(Str.unescape(s.substring(1, s.length()-1)))
	}

	final def cRexp = REXP ^^ { re => {
			// s still contains quotes!
			val s = re.substring(1, re.length()-1)
			Constant(new Rexp(s.replaceAll("\\\\'", "'")))
		}
	}

	final def cDecimal = REAL ^^ {
		s => Constant(
			if (s.contains('.') || s.contains('E') || s.contains('e')){
				// Real
				if (s.endsWith("L")) BigR(BigDecimal(s.substring(0, s.length-1)))
				else try {
					val d = s.toDouble
					if (d.isInfinite || d.isNaN)
						throw new NumberFormatException("Invalid real: " + s)
					Real(d)
				} catch {
					// retry as BigReal
					case _: Throwable => try{ BigR(BigDecimal(s)) } catch {
							case _: Throwable => throw new ParseException("Invalid Real literal: " + s)
						}
				}
			} else {
				// Lint
				if (s.endsWith("L")) BigI(BigInt(s.substring(0, s.length-1)))
				else try { Lint(s.toLong)  } catch {
					// retry as BigInt
					case _: Throwable => try{ BigI(BigInt(s)) } catch {
							case _: Throwable => throw new ParseException("Invalid Int literal: " + s)
						}
				}
			})
	}



	final def cFloat = REAL ^^ {
		s => Constant(
			if (s.endsWith("L")) BigR(BigDecimal(s.substring(0, s.length-1)))
			else try {
				val d = s.toDouble
				if (d.isInfinite || d.isNaN)
					throw new NumberFormatException("Invalid real: " + s)
				Real(d)
			} catch {
				// retry as BigReal
				case _: Throwable => try{ BigR(BigDecimal(s)) } catch {
						case _: Throwable => throw new ParseException("Invalid Real literal: " + s)
					}
			}
		)
	}

	final def cDec = DEC ^^ {
		s => Constant(
			if (s.endsWith("L")) BigI(BigInt(s.substring(0, s.length-1)))
			else try { Lint(s.toLong)  } catch {
				// retry as BigInt
				case _: Throwable => try{ BigI(BigInt(s)) } catch {
						case _: Throwable => throw new ParseException("Invalid Int literal: " + s)
					}
			})
	}

//	final def cHex = HEX ^^ { s => try {
//			Constant{
//				val hex = s.replaceAll("_", "")
//				val (hs, he) = if (hex.startsWith("0x"))
//					(2, hex.length - (if (hex.endsWith("L")) 1 else 0))
//				else (0, hex.length-1)
//				val rhex = hex.substring(hs, he)
//				if (rhex.length > 16 || s.endsWith("L"))
//					BigI(BigInt(rhex, 16))
//				else if (rhex.length == 16) { // work around parsing problem with all-fs
//					val tmp = JLong.parseLong(rhex.substring(0, 15), 16)
//					val last = JLong.parseLong(rhex.substring(15), 16)
//					Lint((tmp << 4) | last )
//				}
//				else
//					Lint(java.lang.Long.parseLong(rhex, 16))
//			}
//		} catch {
//			case _ => throw new ParseException("Invalid hex literal: " + s)
//		}
//	}
	final def cHex = immediate(RHEX) ^^ { s => try {
			Constant{
				val hex = s.replaceAll("_", "")
				val rhex = if (hex.endsWith("L")) hex.substring(0, hex.length-1)
				else hex
				if (rhex.length > 16 || s.endsWith("L"))
					BigI(BigInt(rhex, 16))
				else if (rhex.length == 16) { // work around parsing problem with all-fs
					val tmp = JLong.parseLong(rhex.substring(0, 15), 16)
					val last = JLong.parseLong(rhex.substring(15), 16)
					Lint((tmp << 4) | last )
				}
				else
					Lint(java.lang.Long.parseLong(rhex, 16))
			}
		} catch {
			case _: Throwable => throw new ParseException("Invalid hex literal: " + s)
		}
	}

	// no real point using L here...
	final def cBin = immediate(RBINARY) ^^ {
		s => val bin = s.replaceAll("_", "")
		val result = JLong.parseLong(bin,2)
		Constant(Lint(result))
	}

	// assumes a valid name has been parsed
	final def convertName(name: String): Leaf = {
		if (name == E) Constant(Real(math.E))
		else if (name == PI) Constant(Real(math.Pi))
		else if (TRUE.pattern.matcher(name).matches) Constant(Bool.True)
		else if (FALSE.pattern.matcher(name).matches) Constant(Bool.False)
		else if (name.startsWith("REGEX_")) RxPatterns.getOrElse(
			name.substring(6), Variable(name))
		else TYPENAMES.getOrElse(name, Variable(name))
	}

	// just eats whitespace
	def ws: Parser[Unit] = Parser[Unit] { in => Success((), eatws(in)) }
	def eatws(in: Input): Input =  {
		val source = in.source
		val offset = in.offset
		in.drop(handleWhiteSpace(source, offset) - offset)
	}

	/** Replaces literal from RegexParsers.
	 *
	 * Returning a substring from input instead of the string, we already know
	 * is just plain nonsense and a waste of time.
	 *
	 * Besides being faster, having this here also helps debugging
	 * the whole stuff.
	 * Since the message produced in case of an error will
	 * be outright misleading in most cases anyway, we don't
	 * even try to make it meaningful.
	 */
	override implicit def literal(s: String):
	Parser[String] = new Parser[String] {
		def apply(in: Input) = {
			val source = in.source
			val offset = in.offset
//			println("## Trying: " + s + " at [" +
//					source.subSequence(
//					offset, math.min(source.length, offset + 20)))
			val start = handleWhiteSpace(source, offset)
			val last = s.length - 1
			var i = -1
			var j = start

			// roughly check if that might fit at all
			if (source.length >= start + s.length &&
				s.charAt(last) == source.charAt(start + last)) {

				// might fit, look more closely
				i = 0
				while (i < last && s.charAt(i) == source.charAt(j)) {
					i += 1
					j += 1
				}
			}
			if (i == last){
//				println("## found " + s + "]")
				Success(s, in.drop(j + 1 - offset))
			}
			else
				Failure("No literal match", in.drop(start - offset))
		}
	}

	// Same as literal, but doesn't allow leading whitespace
	def immediate(s: String):
	Parser[String] = new Parser[String] {
		def apply(in: Input) = {
			val source = in.source
			val offset = in.offset
//			println("## Trying: " + s + " at [" +
//					source.subSequence(
//					offset, math.min(source.length, offset + 20)))
			val last = s.length - 1
			var i = -1
			var j = offset

			// roughly check if that might fit at all
			if (source.length >= offset + s.length &&
				s.charAt(last) == source.charAt(offset + last)) {

				// might fit, look more closely
				i = 0
				while (i < last && s.charAt(i) == source.charAt(j)) {
					i += 1
					j += 1
				}
			}
			if (i == last){
//				println("## found " + s + "]")
				Success(s, in.drop(j + 1 - offset))
			}
			else
				Failure("No literal match", in)
		}
	}

	def immediate(r: Regex): Parser[String] = new Parser[String] {
		def apply(in: Input) = {
			val source = in.source
			val offset = in.offset
			(r findPrefixMatchOf (source.subSequence(offset, source.length))
			) match {
				case Some(matched) =>
					Success(source.subSequence(offset, offset + matched.end).toString,
							in.drop(matched.end))
				case None =>
					Failure("`"+r+"' expected, `"+in.first+"' found", in)
			}
		}
	}

	/** A parser that matches a Symbol */
	implicit def symbol(sym: Symbol): Parser[Symbol] = literal(sym.name) ^^^ sym

	// original impl. is not very informative...
	override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser{
		in =>
		println("trying "+ name +" at ["+
				in.source.subSequence(in.offset, in.source.length).toString
				+ "]")
		val r = p(in)
		println(name +" --> "+ r)
		r
	}
}
