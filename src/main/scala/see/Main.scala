/*
 * Main.scala
 *
 */

package see

private[see] object Main {

	/** Debugging into tests doesn't work well.
	 * So we do that here...
	 * 
	 * @param args the command line arguments
	 */
	def main(args: Array[String]) :Unit = {


		println("Testing parser!")
		val see = See.create()
		val prog = """
		1
		"""
		see.set("a", 42)

		val node = see.parse(prog)
		println("Parsed expression: " + node)
		var result = see.eval(node)


		profile(5)

	}


	def profile(runs: Int): Unit = {

		val see = See.create
		val prog = """
		x = { a + 1}
		"""
		see.set("a", 42)
		see.evalAsDouble(prog) // cut off startup issues
		var node: INode = null
		var evals = 0.0
	
		evals = 0.0
		for (n <- 0 until runs){
			var now = System.currentTimeMillis
			while (now == System.currentTimeMillis) {}
			now = System.currentTimeMillis
			while (System.currentTimeMillis < now + 1000 ) {
				see.evalAsDouble(prog)
				evals += 1
			}
		}
		println ("Average parse/eval pairs per second: " + evals / runs	)

		evals = 0.0
		for (n <- 0 until runs){
			var now = System.currentTimeMillis
			while (now == System.currentTimeMillis) {}
			now = System.currentTimeMillis
			while (System.currentTimeMillis < now + 1000 ) {
				node = see.parse(prog)
				evals += 1
			}
		}
		println ("Average parses per second: " + evals / runs	)

		evals = 0.0
		node = see.parse(prog)
		for (n <- 0 until runs){
			var now = System.currentTimeMillis
			while (now == System.currentTimeMillis) {}
			now = System.currentTimeMillis
			while (System.currentTimeMillis < now + 1000 ) {
				see.evalAsDouble(node)
				evals += 1
			}
		}
		println ("Average evals per second: " +	 evals / runs	)

	}
}
