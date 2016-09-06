/*
 *
 */

package see

import values.Val

/** All errors encountered by see will be reported through this exception.
 */
class SeeException(msg: String) extends RuntimeException(msg)

/** Exception for failures encountered during parsing. */
class ParseException(s: String) extends SeeException(s)
private class SyntaxError(s: String) extends ParseException("Syntax Error: " + s)

/** Failures encountered during evaluation */
class EvalError(msg: String) extends SeeException(msg)
private case class Unsupported(any: AnyRef) extends EvalError(
	if (any == null) "Undefined object"
	else "Unsupported type: " + any.getClass.getName + "." )

private case class Unresolved(name: String) extends EvalError("Unresolved name: " + name + ".")
private case class Illegal(op: String) extends EvalError("Illegal operation '" + op + "'.")
private case class Unknown(op: String) extends EvalError("Unknown operation '" + op + "'.")
private case class DivisionByZero() extends EvalError("Division by Zero.")
private case class ParamError(m: String) extends EvalError(m)
private case class RangeOverflow(m: String) extends EvalError(m)

// Used to transfer premature return values.
private case class ResultException(result: Val) extends Throwable
