/*
 *  
 */

package see.nodes

/** The global Precedence table.
*
* I want all precedences in one place to
* ensure they remain consistent.
*
*/
private[see] object PREC {

	val Deref = 50
	val User  = 100
	val Regex = 200
	val SetOps = 290
	val Slice = 300
	val At = 300
	val Vectorize = 350
	val Concat = 360
	val Exp = 400
	val Times = 500
	val BitLshift = 510
	val BitRshift = 520

	val BitAnd = 610
	val BitXor = 620
	val BitOr = 630
	val Plus = 1000
	val Relation = 1200
	val TypeCheck = 1300
	val BoolAnd = 1410
	val BoolXor = 1420
	val BoolOr = 1430
	val Condition = 1600
	val Matcher = 1610
	val Assoc = 2000
	val Anondef = 5000

	val Statement = 10000
	val Assign = 10001
	val Fundef = 10100
	val Return = 12000
	val Assertion = 12010
}
