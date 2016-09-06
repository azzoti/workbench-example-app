package org.lazyluke

import org.scalajs.dom
import org.scalajs.dom._
import see.{ParseException, See}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by azzoti on 20/06/2016.
  */
@JSExport
object Main {
  @JSExport
  def main(canvas: html.Canvas, target: html.Div): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    def dynWidth = Math.max(document.documentElement.clientWidth, dom.innerWidth)
    def dynHeight = Math.max(document.documentElement.clientHeight, dom.innerHeight)

    var windowWidth = 0
    var windowHeight = 0

    var x = 0;


    def resizeCanvas(): Unit = {
      windowWidth = dynWidth
      windowHeight = dynHeight
      ctx.canvas.width = windowWidth
      ctx.canvas.height = windowHeight
    }

    def resetGraph(width: Int, height: Int) = {


      val maxesAndMins: Option[(Double, Double, Double, Double, Double)] = getFormulaMaxesAndMins()
      if (maxesAndMins.isDefined) {
        // val (minX, maxX, stepX, minY, maxY) = maxesAndMins.get
        println(maxesAndMins.get._1)
        println(maxesAndMins.get._2)
        println(maxesAndMins.get._3)
        println(maxesAndMins.get._4)
        println(maxesAndMins.get._5)
      } else {
        println("maxesAndMins undefined")
      }


      Graph.drawAxes(ctx, width, height)
      x = -width / 2
      setFillColor("red")

    }


    def resizeHandler(ev: dom.raw.UIEvent): Unit = {

      resizeCanvas()

      resetGraph(windowWidth, windowHeight)
    }



    def drawPoint(x1: Double, y1: Double): Unit = {
      ctx.fillRect(x1 + windowWidth / 2, windowHeight / 2 - y1, 1, 1)
    }

    def setFillColor(color: String): Unit = {
      //ctx.fillStyle = "white"
      //      val r = x % 255
      //      val g = 255 - r
      //      val b = y.toInt % 255
      ctx.fillStyle = color // s"rgb($g, $r, $b)"
    }


    def getFormulaMaxesAndMins(): Option[(Double, Double, Double, Double, Double)] = {
      FormulaGui.clearError()
      val see = See.create()
      val formula = FormulaGui.formula()
      var minY = Double.MaxValue
      var maxY = Double.MinValue
      try {
        val node = see.parse(formula)
        val start: Option[Double] = FormulaGui.xStartValue()
        val end: Option[Double] = FormulaGui.xEndValue()
        val step: Option[Double] = FormulaGui.xStepValue()
        if (!start.isDefined || !end.isDefined || !step.isDefined) {
          None
        } else {
          for (x <- start.get to end.get by step.get) {
            see.set("x", x)
            val y = see.eval(node).toDouble
            minY = math.min(minY, y)
            maxY = math.max(maxY, y)
          }
          Some((start.get, end.get, step.get, minY, maxY))
        }
      } catch {
        case pe: ParseException =>
          FormulaGui.showError()
          None
      }

    }


    def run() = {

      val see = See.create()
      val formula = FormulaGui.formula()
      //println("Formula: " + formula)
      try {
        val node = see.parse(formula)
        //println("Parsed parse: " + node)
        see.set("x", x)
        val result = see.eval(node)
        // println("Parsed eval: " + result.toDouble)

        val y = result.toDouble * windowHeight * 0.9 / 2

        FormulaGui.clearError()

        drawPoint(x, y)

      } catch {
        case pe: ParseException =>
          FormulaGui.showError()
      }

      if (x > windowWidth / 2) resetGraph(windowWidth, windowHeight)

      x = x + 1

    }


    FormulaGui.renderFormulaHtml(target, resizeHandler)


    dom.onresize = resizeHandler _



    resizeHandler(null)
    dom.setInterval(() => run(), 50)
  }

  def degreesToRadians(degrees: Double): Double = {
    degrees * math.Pi / 180
  }
}
