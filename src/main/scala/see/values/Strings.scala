/*
 *  String and Regex types
 */

package see.values

import java.util.regex.Pattern

import scala.collection.mutable.ListBuffer

//import see.ConstParser
import see.{ParseException, Scope}

//import see.StableScope


/* A sequence of characters. */
private[see] case class Str(v: String) extends Comparable {
  type T = String

  override def selType = 'String

  override def isType(typeId: Symbol) = (typeId == 'String) || super.isType(typeId)

  override def toBool: Boolean = !v.isEmpty

  override def toString = "\"" + toStr + "\""

  override def toStr = v

  override def toJava = v

  override def size = v.length

  // comparing strings to numbers may yield strange results,
  // since "-123" > "-122", therefore we try number conversion first.
  // However if both operands are strings to begin with,
  // the above relation still holds.
  override def propagate(other: Comparable) = other match {
    case on: Number => toNumber.map(_ propagate on).getOrElse(this)
    case _ => this
  }

  def toNumber: Option[Number] = try {
    val s = Scope.clean
    val node = s.parse(v)
    (node evalIn s) match {
      case x: Number => Some(x)
      case _ => None
    }
  } catch {
    case _: Throwable => None
  }

  override def cmp(rhs: Comparable): Int = rhs match {
    // ensure, we use number comparison wherever possible:
    case n: Number => toNumber.map(_ propagate n) match {
      case Some(x) => x cmp n
      case _ => v compare rhs.toStr
    }
    case _ => v compare rhs.toStr
  }

  override def fits(destType: Class[_]) =
    if (destType.isAssignableFrom(classOf[String])) 10 else 0
}

private[see] object Str {

  // It's incredible. We have to roll our own.
  // Taken from Apaches StringEscapeUtils
  def unescape(in: String) = {
    val out = new StringBuilder(in.size)
    val unicode = new StringBuilder(4)
    var uci = -1
    var hadSlash = false
    for (ch <- in) {
      if (uci >= 0) {
        unicode += ch
        uci += 1
        if (uci == 4) try {
          val cp = Integer.parseInt(unicode.toString(), 16)
          out += cp.asInstanceOf[Char]
          uci = -1
          hadSlash = false
          unicode.setLength(0)
        } catch {
          case _: Throwable => throw new ParseException(
            "Invalid Unicode escape '" + unicode + "'.")
        }
      } else if (hadSlash && ch == 'u') uci = 0 // start unicode seq
      else if (hadSlash) {
        hadSlash = false
        out += (ch match {
          case 'r' => '\r'
          case 'f' => '\f'
          case 't' => '\t'
          case 'n' => '\n'
          case 'b' => '\b'
          case x => x
        })
      } else if (ch == '\\')
        hadSlash = true
      else
        out += ch
    }
    if (hadSlash) // actually illegal
      out += '\\'
    Str(out.toString)
  }
}

/* A regular expression. Should behave like a string in most cases. */
private[see] case class Rexp(v: Pattern) extends Comparable {
  type T = Pattern

  override def selType = 'Regex

  override def isType(typeId: Symbol) = (typeId == 'Regex) || super.isType(typeId)

  def this(s: String) = this(Pattern.compile(s))

  // standard equals doesn't work as expected
  override def equals(other: Any) =
    other.isInstanceOf[Rexp] && v.pattern == other.asInstanceOf[Rexp].v.pattern

  override def toBool: Boolean = true

  override def toString = "'" + toStr + "'"

  override def toStr = v.pattern

  override def toJava = v

  override def size = v.pattern.length

  // supported operations:
  // full match test
  def ~(rhs: Val): Bool = Bool(v.matcher(rhs.toStr).matches)

  // extraction into string vector
  def ~~(rhs: Val): Vector = {
    val m = v.matcher(rhs.toStr)
    if (m.matches) {
      val groups = for (n <- 0 to m.groupCount) yield Str(m.group(n))
      Vector(groups)
    }
    else Vector.Empty
  }

  // Reports string index after first match
  def ~#(rhs: Val): Lint = {
    val m = v.matcher(rhs.toStr)
    if (m.find) Lint(m.end)
    else Lint(-1)
  }

  // Reports first match as string vector
  def ~+(rhs: Val): Vector = {
    val m = v.matcher(rhs.toStr)
    if (m.find) {
      val groups = for (n <- 0 to m.groupCount) yield Str(m.group(n))
      Vector(groups)
    }
    else Vector.Empty
  }

  // extraction into string vector
  def ~*(rhs: Val): Vector = {
    val m = v.matcher(rhs.toStr)
    val matches = ListBuffer[Vector]()
    while (m.find) {
      val groups = for (n <- 0 to m.groupCount) yield Str(m.group(n))
      matches += Vector(groups)
    }
    Vector(matches.toList)
  }

  // we always propagate this to a string,
  // if used within some operation other than match
  // (no number conversion support)
  override def propagate(other: Comparable) = Str(toStr)

  override def cmp(rhs: Comparable): Int = toStr compare rhs.toStr


  override def convertTo(destType: Class[_]): AnyRef =
    if (destType.isAssignableFrom(classOf[Pattern])) v
    else super.convertTo(destType)

  override def fits(destType: Class[_]) =
    if (destType.isAssignableFrom(classOf[Pattern])) 10
    else if (destType.isAssignableFrom(classOf[String])) 5 else 0
}

