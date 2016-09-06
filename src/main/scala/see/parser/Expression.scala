/*
 *  
 */

package see.parser

import see.Binary
import see.ParseException
import see.Relator
import see.Scope
import see.Unary
import see.nodes._
import see.values._
import see.operations._

private[see] object Expressions {

	private type E = Expressions
	abstract class Generator (val precedence: Int )
	{
		def operandParser(e: E): E#Parser[Proto]
	}

	private class G(p: Int)
	(private val parser:  E => E#Parser[Proto])
	extends Generator(p){
		def operandParser(e: E) = parser(e)
	}

	def oneOperand(nf: Factory) = (e: E) => { e.operand ^^ { nf(_) }}

	// generates operational infix node with arbitrary operator
	private def binary(o: Binary, prec: Int) = 
		new G(prec)(oneOperand(Operation(o,prec)))

	// generates operational infix with relation operator
	private def genRel(o: Relator) = 
		new G(PREC.Relation)(oneOperand(Relation(o)))

	def node(nf: Factory): Generator =
		new G(nf(EmptyNode).precedence)(oneOperand(nf))

	private val TOKENS = Map[String, Generator](

		FindAll.opCode -> binary(FindAll, PREC.Regex),
		FindFirst.opCode -> binary(FindFirst, PREC.Regex),
		FindFirstPos.opCode -> binary(FindFirstPos, PREC.Regex),
		Selection.opCode -> binary(Selection, PREC.Regex),

		SetUnion.opCode -> binary(SetUnion, PREC.SetOps),
		SetIntersection.opCode -> binary(SetIntersection, PREC.SetOps),
		SetDifference.opCode -> binary(SetDifference, PREC.SetOps),

		Slice.opCode -> binary(Slice, PREC.Slice),
		At.opCode -> binary(At, PREC.At),

		Vectorize.opCode -> binary(Vectorize, PREC.Vectorize),
		Concat.opCode -> binary(Concat, PREC.Concat),

		Exp.opCode -> binary(Exp, PREC.Exp),
		"EXP" -> binary(Exp, PREC.Exp),
		Gcd.opCode -> binary(Gcd, PREC.Times),
		ScalProd.opCode -> binary(ScalProd, PREC.Times),
		Times.opCode -> binary(Times, PREC.Times),
		Mod.opCode -> binary(Mod, PREC.Times),
		Div.opCode -> binary(Div, PREC.Times),

		// shift works like mul/div, therefore we place it here
		BitLshift.opCode -> binary(BitLshift, PREC.BitLshift),
		"LSHIFT" -> binary(BitLshift, PREC.BitLshift),
		BitRshift.opCode -> binary(BitRshift, PREC.BitRshift),
		"RSHIFT" -> binary(BitRshift, PREC.BitRshift),

		BitAnd.opCode -> binary(BitAnd, PREC.BitAnd),
		"and" -> binary(BitAnd, PREC.BitAnd),
		BitXor.opCode -> binary(BitXor, PREC.BitXor),
		"xor" -> binary(BitXor, PREC.BitXor),
		BitOr.opCode -> binary(BitOr, PREC.BitOr),
		"or" -> binary(BitOr, PREC.BitOr),

		Plus.opCode -> binary(Plus,  PREC.Plus),
		Minus.opCode -> binary(Minus, PREC.Plus),

		"EQ" -> genRel(Equal),
		Equal.opCode -> genRel(Equal),
		"NE" -> genRel(Ne),
		Ne.opCode -> genRel(Ne),
		"EEQ" -> genRel(ExactEqual),
		ExactEqual.opCode -> genRel(ExactEqual),
		"NEE" -> genRel(ExactNe),
		ExactNe.opCode -> genRel(ExactNe),
		"GT" -> genRel(Greater),
		Greater.opCode -> genRel(Greater),
		"LT" -> genRel(Less),
		Less.opCode -> genRel(Less),
		"GE" -> genRel(Ge),
		Ge.opCode -> genRel(Ge),
		"LE" -> genRel(Le),
		Le.opCode -> genRel(Le),
		BoolMatch.opCode -> genRel(BoolMatch),
		StrongContainment.opCode -> genRel(StrongContainment),
		WeakContainment.opCode -> genRel(WeakContainment),

		TypeCheck.opCode -> binary(TypeCheck, PREC.TypeCheck),
		BoolAnd.opCode ->  binary(BoolAnd, PREC.BoolAnd),
		"AND" -> binary(BoolAnd, PREC.BoolAnd),
		BoolXor.opCode ->  binary(BoolXor, PREC.BoolXor),
		"XOR" -> binary(BoolXor, PREC.BoolXor),
		BoolOr.opCode ->  binary(BoolOr, PREC.BoolOr),
		"OR" -> binary(BoolOr, PREC.BoolOr),

		"->" -> node(Association)
	)

	private[see] trait AtomGenerator
	{
		def apply(): Proto
	}
	private class PrefixGen (val op: Unary) extends AtomGenerator
	{
		final def apply() = new PrefixP(op)
	}
	private def prefix(o: Unary) = new PrefixGen(o)

	private val PREFIXES = Map[String, AtomGenerator](
		"+" -> prefix(UnaryPlus),
		"-" -> prefix(UnaryMinus),
		"!" -> prefix(UnaryNot),
		"~" -> prefix(UnaryInv),
		"NOT" -> prefix(UnaryNot),
		"not" -> prefix(UnaryInv),
		"`" -> prefix(Symbolize),
		"defined" -> new AtomGenerator {
			def apply() = new DefinedP()
		},
		"local" -> new AtomGenerator {
			def apply() = new LocalP()
		},
		VarType.opCode -> prefix(VarType)
	)
}


private[see] class Expressions extends Literals {

	import Literals._
	import Expressions._

	type NP = Parser[Node]

	def all: NP = expression

	protected class Top(var node: Node, var at: Input)
	protected var state: Top = null

	final def expression(init: Node) = new NP {
		override def apply (in: Input) = {
			val prevState = state
			state = new Top(init, in)
			try {
				parseOperations(Success(init, in))
				Success(state.node, state.at)
			} finally {
				state = prevState
			}
		}
	}

	// a full expression
	final def expression = new NP {
		override def apply (in: Input) = {
			val prevState = state
			state = new Top(null, in)
			try {
				var result = operand(in)
				if (!result.successful)
					Failure("No operand", in)
				else{
					state.node = result.get
					state.at = result.next
					parseOperations(result)
					Success(state.node, state.at)
				}
			} finally {
				state = prevState
			}
		}
	}

	// parses a sequence of token/operand pairs
	private final def parseOperations(from: ParseResult[Node]) = {
		var result = from
		while (result.successful) {
			state.at = result.next
			val opResult = operator(state.at) match {
				case Success(gen, next) => generate(next, gen)
				case NoSuccess(reason, _) => Failure(reason, state.at)
			}
			if (opResult.successful && insert(opResult.get))
				result = Success(state.node, opResult.next)
			else result = Failure("Invalid operator", state.at)
		}
		result
	}

	def operator(name: String): Option[Generator] = TOKENS.get(name)

	private final def loperator = log(operator)("operator")
	private final def operator: Parser[Generator] = Parser {
		in => {
			val nin = eatws(in)
			immediate(OPERATOR)(nin) match {
				case Success(t, next) => operator(t) match {
						case Some(gen) => Success(gen, next)
							// could have been operator / prefix, so trace back
						case _ => traceback(t, nin)
					}
				case _ => immediate(NAME)(nin) match {
						case Success(name, next) => operator(name) match {
								case Some(gen) => Success(gen, next)
								case _ => Failure(
										"Unknown operator name: " + name, nin)
							}
						case _ => Failure("No operator ", nin)
					}

			}}}

	private final def traceback(t: String, fb: Input):
	ParseResult[Generator] = {
		var st = t;
		while(st.length > 1) {
			st = st.dropRight(1)
			val g = operator(st)
			if (g != None) return Success(g.get, fb.drop(st.length))
		}
		Failure("Unknown operator: "+ t, fb)
	}

	private final def loperand = log(operand)("operand")
	final def operand: Parser[Leaf] = Parser { in => {
			val next = eatws(in)
			if (next.atEnd)
				Failure("EOI reached", next)
			else {
				val c = next.source.charAt(next.offset)
				if (c.isDigit) numeric(next) // plain numbers should be fast
				else prefixed(next) match {
					case Success(l: Leaf, next1) => call(l)(next1)
					case _ => constant(next)
				}
			}
		}
	}

	private final def prefixed: Parser[Leaf] = Parser { in => {
			if (in.atEnd) Failure("EOI", in)
			else {
				val c = in.source.charAt(in.offset)
				prefix(c.toString) match {
					case Some(gen) => nextPrefix(gen(), in.drop(1))
					case _ => name(in)  match {
							// prefix by name?
							case leaf @ Success(Variable(n), next2) =>
								prefix(n) match {
									case Some(gen) => nextPrefix(gen(), next2)
									case _ => leaf
								}
								// some other name
							case ok @ Success(_, next2) => ok
								// not a name, try other construct
							case _ => atom(in)
						}
				}
			}
		}
	}

	def nextPrefix(pre: Proto, in: Input): ParseResult[Leaf] = {
		prefixed(eatws(in)) match {
			case (e: NoSuccess) => Error("Missing operand", in)
			case Success(leaf, next) => pre.finish(leaf) match {
					case Some(l: Leaf) => Success(l, next)
					case _ => Error("Invalid prefix operand: " + leaf, in)
				}
		}
	}

	def prefix(name: String) = PREFIXES.get(name)

	def atom: Parser[Leaf] = vector | constant

	final def vector: Parser[Leaf] =
		"(" ~> repsep(expression, ",") <~ ")" ^^ { Vnode(_) }

	final def call(base: Leaf) = Parser {
		in => vector(in) match {
			case Success(arg, next) => {
					var result: List[Leaf] = base match {
						case Variable(name) =>
							List(new Fcall(name, arg))
						case other =>
							List(base, new Fcall(Scope.RNAME, arg))
					}
					var nextArg = vector(next)
					while (nextArg.successful) {
						result = result :::
						List( new Fcall(Scope.RNAME, nextArg.get))
						nextArg = vector(nextArg.next)
					}
					if (result.size > 1)
						Success(Nodes(result), nextArg.next)
					else Success(result.head, next)
				}
			case _ => Success(base, in)
		}
	}

	// a context-free name
	final def name: Parser[Leaf] = Parser{
		in => regex(NAME)(in) match {
			case f: Failure => f
			case Success(n, next) => Success(convertName(n), next)
			case _ => throw new ParseException("Should not get here")
		}
	}

	private final def generate (in: Input, gen: Generator):
	ParseResult[Proto] = {
		//println (gen)
		val parser = Parser{ in => (
				gen.operandParser(this).asInstanceOf[this.Parser[Proto]])(in)
		}
		val x = parser(in)
		//println (x)
		x match {
			case Success(n: Proto, next) => Success(n, next) // type remap obscure, but necessary
			case NoSuccess(reason, _) => Failure(reason, state.at) // also discard operator
		}
	}

	// Inserts node into tree. 
	// Any failure to do so will result in a parsing error.
	private final def insert(proto: Proto): Boolean = {
//		println ("Inserting " + proto)
//		println(" into  " + state.node)
		if (proto.precedence >= state.node.precedence)
			proto.finish(state.node).exists(n=>{state.node = n; true})
		else state.node match {
			case b: Branch => b.rhs.insert(b, proto)
			case n => proto.finish(n).exists(n=>{state.node = n; true})
		}
	}
}

