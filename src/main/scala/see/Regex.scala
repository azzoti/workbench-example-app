/*
 *  
 */

package see

// I don't really understand why something like this
// is not part of scalas Regex.
// unapplySeq is just fine, but too heavyweight,
// if we just want to test for a match and aren't interested in groups.
private[see] class Regex(s: String ) extends scala.util.matching.Regex(s) {

	def matches(tgt: String) = pattern.matcher(tgt).matches
	def matches(tgt: Symbol) = pattern.matcher(tgt.name).matches
}
