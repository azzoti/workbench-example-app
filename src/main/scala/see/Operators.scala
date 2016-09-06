/*
 * Operator.scala
 *
 */

package see

import java.io.File
import java.io.FileInputStream
//import java.util.jar.JarEntry
import java.util.jar.JarInputStream

import nodes.Node
import values._
import operations._


private abstract class Operator (val opCode: String) {
}


/** I am too lazy to maintain operator tables manually.
 *
 * Therefore, this object will load all operators at runtime from package operations.
 * Takes quite long (nearly half a sec. on my machine),
 * but since it's a one time op, I don't care.
 *
 * Only works, if operator class is within build directory or see jar.
 */
private object Operator {
	private var preloaded = true // false

	private object classFilter extends java.io.FilenameFilter {
		def accept(dir: java.io.File, name: String): Boolean =
			name.endsWith(".class")
	}

	// Loads any operators defined within package operations into their lookup maps.
	def preload() {
//		if (preloaded)	return
//		preloaded = true;
//		val myClass = getClass
//		val loader = myClass.getClassLoader
//		val packageName = myClass.getPackage.getName + ".operations"
//        val path = packageName.replace('.', '/')
//        val url = loader.getResource(path)
//		if (url == null) return
//		var packagePath = url.getFile
//		if (packagePath == null) return
//		// WINDOWS HACK: replace escaped whitespace
//		if (packagePath.indexOf("%20") > 0)
//			packagePath = packagePath.replaceAll("%20", " ")
//		if (packagePath.indexOf("!") < 0 || packagePath.indexOf(".jar") < 0)
//			loadDirectory(packagePath, packageName)
//		else {
//			var jarPath = packagePath.substring(0, packagePath.indexOf(
//					"!")).substring(packagePath.indexOf(":") + 1);
//			// WINDOWS HACK: skip leading backspace
//			if (jarPath.contains(":"))
//				jarPath = jarPath.substring(1)
//			loadJAR(jarPath, path)
//		}
		() // return unit
	}

	// Since we only want to load classes, there is no point in listing them explicitly

	private def loadDirectory(dirName: String, packageName: String) {
		val dir = new File(dirName)
		if (dir.exists)
			for (file <- dir.list; if file.endsWith(".class")) {
				val name = packageName + '.' +
				file.substring(0, file.lastIndexOf('.'))
				// performs actual load. Map entry is generated as side effect,
				// which causes a permanent reference, so it won't be unloaded again.
				Class.forName(name)
			}
	}

	private def loadJAR(jar: String, packageName: String ) {
		val jarFile = new JarInputStream(new FileInputStream(jar))
		var jarEntry = jarFile.getNextJarEntry
		while (jarEntry != null) {
			var className = jarEntry.getName
			if (className endsWith ".class") {
				className = className.substring(0, className lastIndexOf '.')
				if (className.startsWith(packageName))
					Class.forName(className.replace('/', '.'))
			}
			jarEntry = jarFile.getNextJarEntry
		}
	}

}

private class Unary(opc: String) extends Operator(opc) {
	//	println("Class " + getClass.getName + " loaded")
//	unaryOperators.map.put(opCode, this)

	// calculate result from operand within a certain scope
	// Most operators will just ignore the scope.
	def apply(s: Scope, v: Val): Val = v.coerce match {
		case c: Container => c.map(v => apply(s, v))
			// try specialized code again, in case coerce had any effect
		case v1 if (!(v1 eq v)) => apply(s, v1)
			// otherwise we cannot handle the operand
		case _ => throw new Illegal(opCode)
	}

}


/** Defines a binary operation of two values.
 */
private class Binary (opc: String) extends Operator(opc) {

	def needsRhs(lhs: Val) = true

	def isDefinedFor(s: Scope, lhs: Node, rhs: Node) =
		(lhs isDefinedIn s) && (rhs isDefinedIn s)

	def apply(lhs: Val, rhs: Val): Val = {
		(lhs.coerce, rhs.coerce) match {
			// may happen, if any operand was a closure:
			case (l: Scalar, r: Scalar) if (!(l eq lhs) || !(r eq rhs) ) =>
				apply(l, r)
			case (c: Container, s: Scalar) => c.map(x => apply(x.coerce, s))
			case (s: Scalar, c: Container) => c.map(y => apply(s, y.coerce))

			// vector/vector operations are special.
			case (Vector(x), Vector(y)) => Vector(
					for ((k,j) <- x zip y) yield apply(k.coerce, j.coerce))

				// doesn't make sense for other container combis
			case (c: Container, Vector(ys)) => c.map(x =>
					Vector(for (y <- ys) yield apply(x.coerce, y.coerce)))
			case (Vector(xs), c: Container) => c.map(y =>
					Vector(for (x <- xs) yield apply(x.coerce, y.coerce)))
			case _ => throw new Illegal(opCode)
		}
	}
}


// Operator working on any kind of number. For vectors, recursion is applied.
private class BinNumOp (opc: String)(op: (Number, Number) => Val)
extends Binary(opc) {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (l : Number, r: Number) => op(l, r)
			case _ => super.apply(lhs, rhs)
		}
}

// Operator working on Integers only. For vectors, recursion is applied.
private class BinIntOp (opc: String)(op: (IntLike, IntLike) => IntLike)
extends Binary(opc) {

	override def apply(lhs: Val, rhs: Val): Val =
		(lhs, rhs) match {
			case (l: IntLike, r: IntLike) => op(l,r)
			case _ => super.apply(lhs, rhs)
		}
}

private abstract class Relator(opc: String) extends Binary(opc) {
//	override def combine(opNode: Node, newR: Node): Infix =
//		Infix(BoolAnd, opNode,
//			  Infix(this, opNode.asInstanceOf[Infix].rhs, newR))
}

// Built-in functions that may be overwritten
private object builtins {
	def contains(opCode: String) = map.contains(opCode)
	def apply(opCode: String): Option[Unary] = map.get(opCode)
//	val map = scala.collection.mutable.Map[String, Unary]()
	val map = Map[String, Unary](
		Abs.opCode -> Abs,
		Ceil.opCode -> Ceil,
		Floor.opCode -> Floor,
		Round.opCode -> Round,
		ToInt.opCode -> ToInt,
		ToReal.opCode -> ToReal,
		ToBig.opCode -> ToBig,
		ToBigI.opCode -> ToBigI,
		ToBool.opCode -> ToBool,
		ToStr.opCode -> ToStr,
		ToRexp.opCode -> ToRexp,
		ToVect.opCode -> ToVect,
		ToAssoc.opCode -> ToAssoc,
		GetValues.opCode -> GetValues,
		GetKeys.opCode -> GetKeys,
		ToMap.opCode -> ToMap,
		ToTable.opCode -> ToTable,
		Length.opCode -> Length,
		Rnd.opCode -> Rnd,
		Sqrt.opCode -> Sqrt,
		Log.opCode -> Log,
		Log10.opCode -> Log10,
		Power.opCode -> Power,
		Sin.opCode -> Sin,
		Cos.opCode -> Cos,
		Tan.opCode -> Tan,
		ASin.opCode -> ASin,
		ACos.opCode -> ACos,
		ATan.opCode -> ATan,
		Max.opCode -> Max,
		Min.opCode -> Min,
		Sum.opCode -> Sum,
		Prod.opCode -> Prod,
		Mean.opCode -> Mean,
		Fold.opCode -> Fold,
		Rep.opCode -> Rep,
		Pad.opCode -> Pad,
		Zip.opCode -> Zip,
		Sort.opCode -> Sort,
		Unique.opCode -> Unique,
		Distinct.opCode -> Distinct,
		Version.opCode -> Version
	)
}


