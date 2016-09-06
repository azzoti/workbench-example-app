/*
 *  
 */

package see.parser

import see.SyntaxError
import see.nodes._
import see.operations._

private object Program {

  import Expressions._

  val SSEP0 = ";?[;\\s]*".r
  // optional ampersand
  val SSEP1 = ";[;\\s]*".r

  // required ampersand
  private val TOKENS = Map[String, Generator](


    "?" -> cond(Condition),
    "??" -> cond(Loop),
    "?~" -> new G(PREC.Matcher)(_.patternmatch),

    "=" -> node(Assign),
    "+=" -> node(ReAssign(Plus)),
    "*=" -> node(ReAssign(Times)),
    "-=" -> node(ReAssign(Minus)),
    "%=" -> node(ReAssign(Mod)),
    "/=" -> node(ReAssign(Div)),
    ">>=" -> node(ReAssign(BitRshift)),
    "<<=" -> node(ReAssign(BitLshift)),
    "^=" -> node(ReAssign(BitXor)),
    "|=" -> node(ReAssign(BitOr)),
    "&=" -> node(ReAssign(BitAnd)),

    "=>" -> new G(PREC.Anondef)(_.block ^^ {
      new FnodeP(_)
    }),
    ":=" -> new G(PREC.Fundef)(_.block ^^ {
      new FundefP(_)
    }),
    "?=" -> stmtOpd(Return),
    "?!" -> stmtOpd(Assertion),
    "assert" -> stmtOpd(Assertion)
  )

  private def cond(nf: Factory2) =
    new G(nf(EmptyNode, EmptyNode).precedence)(_.lc_rhs(nf))

  private def stmtOpd(nf: Factory) =
    new G(nf(EmptyNode).precedence)(_.statement(nf))

  private class G(p: Int)
                 (private val parser: Program => Expressions#Parser[Proto])
    extends Expressions.Generator(p) {
    def operandParser(e: Expressions) = parser(e.asInstanceOf[Program])
  }
}

private[see] class Program extends Expressions {

  import Program._

  override def all: Parser[Node] = statements

  override def operator(op: String) =
    TOKENS.get(op).orElse(super.operator(op))

  override def atom: Parser[Leaf] = block | super.atom

  final def block = ("{" ~> statements ~ opt("}!" ~> statements) <~ "}") ^^ {
    case s ~ None => new Block(s)
    case s ~ Some(cf) => new CatchBlock(s, cf)
  }

  final def lstatements = log(statements)("statements")

  final def statements: Parser[Node] = SSEP0 ~> opt(stms) ^^ {
    case None => EmptyNodes // always use same object here
    case Some(Seq(x)) => x // just one node, optimize right here
    case Some(l) => Nodes(l)
  }

  // nonempty list of statements (last with optional separator)
  final def stms: Parser[List[Node]] = stms1 <~ SSEP0

  final def stms1: Parser[List[Node]] =
    (statement ~ rep(SSEP1 ~> statement)) ^^ {
      case head ~ tail => head :: tail
    }

  final def lstms = log(stms)("stms")

  final def lstatement = log(statement)("statement")

  final def statement(nf: Factory): Expressions#Parser[Proto] =
    statement ^^ {
      nf(_)
    }

  final def statement = expression

  // ==========================================================
  // conditional / loop statement: c ?|?? t : f
  final def lc_rhs(nf: Factory2): Expressions#Parser[Proto] =
    statements ~ ":" ~ statements ^^ { case t ~ ":" ~ f => nf(t, f) }

  // ==========================================================
  // matcher ::= selector ?~ pattern -> result : pattern -> result : ... ;
  final def patternmatch: Parser[Proto] =
    rep1sep(alternative, ":") ^^ {
      new MatcherP(_)
    }

  final def alternative: Parser[Alternative] =
    ((("?" ~ "->") ~> expression) ^^ {
      node => new Alternative("?", node)
    }) | expression ^^ {
      case assoc: Association => new Alternative(assoc.lhs, assoc.rhs)
      case _ => throw new SyntaxError("Unknown match alternative.")
    }


  // A block starts a new scope. It may contain local fundefs!
  final def lblock = log(block)("block")
}

