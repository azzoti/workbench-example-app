/*
 *  
 *  $Id: Main.java 47 2010-09-22 09:25:50Z colomam $
 */
package seec;

import see.INode;
import see.IResult;
import see.See;
import see.SeeException;

import java.io.*;

/** A small console application to try out See interactively.
 * Also demonstrates basic see usage from Java.
 *
 */
public class Main
{

	static See see;
	static boolean error = false;

	static void help()
	{
		System.out.println();
		System.out.println("See command line front end v.0.1.");
		System.out.println("  Usage: see [-c][--help] [-f file] [expr ...] [-i]");
		System.out.println("  Options:");
		System.out.println("    --help  Shows this help text");
		System.out.println(
				"    -c      Uses const parser. Only recognized as first option.");
		System.out.println(
				"    -f file Parses and evaluates whole file as a single program.");
		System.out.println(
				"    -i      Enters interactive mode. Any following arguments are ignored.");
		System.out.println("  Arguments:");
		System.out.println(
				"    expr   Expressions that will be parsed and evaluated one by one.");
		System.out.println(
				"           All evaluations are evaluated within the same context.");
		System.out.println();
		System.out.println(
				"While in interactive mode, an empty input line shows the current context.");
		System.out.println();
		System.out.println();
	}

	static void dump(String varName)
	{
		Object var = see.get(varName);
		if (var == null)
			System.out.println(varName + " is not defined.");
		else {
			if (var instanceof IResult)
				System.out.println(varName + " is " + ((IResult)var).dump());
			else
				System.out.println(varName + " is " + var);
		}
	}

	static void process(String s)
	{
		INode n;
		try {
			n = see.parse(s);
		} catch (SeeException ex) {
			System.err.println("Failed to parse '" + s + "': " + ex);
			error = true;
			return;
		}

		System.out.println("Parsed Node: " + n);

		try {
			IResult result = see.eval(n);
			System.out.println("Result: " + result);
		} catch (SeeException ex) {
			System.err.println("Failed to evaluate '" + s + "': " + ex);
			error = true;
			return;
		}
	}

	static void inputLoop()
	{
		System.out.println("Interactive See Console v.1.0");
		System.out.println("using See v." + See.getVersion());
		System.out.println("Press <Ctrl-C> to exit.");
		System.out.println("");

		try {
			BufferedReader reader =
					new BufferedReader(new InputStreamReader(System.in));

			while (true) {
				System.out.print("see >");
				String expr = reader.readLine();
				if (expr == null)
					break;

				if (expr.equals("help") || expr.equals("?"))
					help();
				else if (expr.startsWith("dump"))
					dump(expr.substring(4).trim());
				else if (expr.isEmpty())
					System.out.println("Current scope: " + see);
				else
					process(expr);
			}
		} catch (IOException ex) {
			// just ignore and exit
		}
		System.exit(0);
	}

	static void evalFile(String name)
	{
		File file = new File(name);
		if (!file.canRead()) {
			System.err.println("File not found: " + file);
			System.exit(1);
		}
		try {
			StringBuilder sb = new StringBuilder();
			BufferedReader r = new BufferedReader(new FileReader(file));
			while (true) {
				String s = r.readLine();
				if (s == null)
					break;

				// if the line starts with a backslash
				if (s.startsWith("\\")) {
					// process current input and restart
					process(sb.toString());
					sb = new StringBuilder();
				}
				else {
					sb.append(s);
					sb.append("\n");
				}
			}
			r.close();
			process(sb.toString());
		} catch (IOException ex) {
			System.err.println("" + ex);
			System.exit(1);
		}
	}

	/**
	 * @param args the command line arguments
	 */
	public static void main(String[] args)
	{
		see = See.create();
		// enter interactive mode, if called without arguments
		if (args.length == 0)
			inputLoop();
		else {
			int n = 0;
			while (n < args.length) {
				if (args[n].equals("--help"))
					help();
				else if (args[n].equals("-c")) {
					see = See.createConst();
				}
				else if (args[n].equals("-f") && n < args.length - 1)
					evalFile(args[++n]);
				else if (args[n].equals("-i"))
					inputLoop();
				else if (args[n].startsWith("-"))
					System.err.println("Unknown option: " + args[n]);
				else {
					try {
						IResult r = see.eval(args[n]);
						System.out.println("" + r);
					} catch (SeeException ex) {
						error = true;
						System.err.println("Failed to evaluate expression: "
								+ ex);
					}
				}
				n += 1;
			}
		}
		// 2 indicates evaluation error
		System.exit(error ? 2 : 0);
	}

	private Main()
	{
	}

}
