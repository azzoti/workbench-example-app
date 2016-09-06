/*
 * Bindings.scala
 */

package see

/** A Resolver enables See to perform sone kind of I/O.
 * It is a pure trait to allow subclassing from Java as regular interface.
 *
 * A user may provide a custom implementation for this interface and
 * set it as parent of a See context. Any variable name thst cannot be resolved
 * from the context itself will then be looked up within the resolver every time
 * the variable is mentioned within an expression.
 * The resolver implementation could for example read some measurement device,
 * depending on the name, and return its output.
 */
trait Resolver {

	/** Retrieves value for a name.
	 * @param name Variable name to look up
	 * @return Value bound to that name.
	 * If the implementation cannot provide a value, it should return null.
	 */
	def get(name: String): AnyRef

	/** Associates a value with a name.
	 * @param name Variable name to (re-)define
	 * @param v Value to associate with the name.
	 * An implementation is not required to allow value setting.
	 * If it doesn't, it should silently ignore the request.
	 */
	def set(name: String, v: AnyRef): Unit

	/** Checks, if the resolver will accept a name.
	 * @param name Variable name to look up.
	 * @return True, if Resolver will provide a value for the name, otherwise false.
	 */
	def contains(name: String): Boolean
}

/** Included mostly for demonstration purposes.
 *
 * See doesn't contain a print statement.
 * This resolver could be used to emulate one.
 * E.g. after setting this as parent, '#x = 10' would output "x = 10".
 */
object Printer extends Resolver {

	// cannot provide any value
	override def get(name: String) = null

	def set(name: String, v: AnyRef) = {
		// strip leading '#'
		println (name.substring(1) + " = " + v)
	}

	def contains(name: String) = name startsWith "#"
}

/** A Binding contains a mapping of variable names to actual values.
 * The mapping may change during expression evaluation!
 */
trait Binding extends Resolver{
	// Using just var parent: Resolver would be fine from a Scala point of view,
	// but it looks strange from Java, so we do:
	/** Sets the parent resolver for the binding.
	 * Any name reference that cannot be resolved locally will be
	 * forwarded to the parent resolver.
	 * @param r Resolver to use as parent. May be null.
	 */
	def setParent(r: Resolver): Unit

	/** Retrieves the parent resolver.
	 * @return Current parent resolver, may be null.
	 */
	def getParent: Resolver

	/** Defines a name within current resolver.
	 *  The name association will be created within local scope,
	 *  even if the parent resolver contains a definition for that name.
	 * @param name Variable name to (re-)define
	 * @param  v Value to associate with the name.
	 * May be null, in which case the name is still regarded as defined,
	 * but will cause an evaluation error, if it is dereferenced.
	 *
	 */
	def setLocal(name: String, v: AnyRef): Unit

	/** Retrieves a set of locally defined names.
	 * @return Set of names that are defined within local scope.
	 * The set may be empty.
	*/
	def getNames(): java.util.Set[String]

	/** Removes all names from local scope.
	 *  Names from a parent resolver are not affected.
	 */
	def clear(): Unit
}

/* Resolver that throws if any access happens.
 Used to test for unexpected evaluations.
 */
private[see] class GuardResolver extends Resolver
{
	override def get(name: String): Nothing = {
		throw new SeeException("Tried to get " + name) }
	override def set(name: String, v: AnyRef): Nothing = {
		throw new SeeException("Tried to set " + name) }
	override def contains(name: String): Boolean = true
}
