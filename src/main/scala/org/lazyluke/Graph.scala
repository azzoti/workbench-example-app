package org.lazyluke

import org.scalajs.dom._
import org.scalajs.dom.ext._


object Graph {


  def drawAxes(ctx: CanvasRenderingContext2D, width: Int, height: Int) = {

    val minX: Int  = -width / 2
    val maxX: Int = width / 2
    val minY: Int  = -height / 2
    val maxY: Int  = height / 2

    val zeroZeroOnCanvas = Point(maxX, maxY)

    def tx(from: Point) : Point = {
      // translate from a maths point to a canvas point
      Point(from.x + zeroZeroOnCanvas.x, zeroZeroOnCanvas.y - from.y)
    }

    val tickLength: Int = 4
    val tickInterval: Int = 10
    val bigTickEvery: Int = 10

//    def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
//      val p1 = tx(Point(x1, y1))
//      val p2 = tx(Point(x1, y1))
//      ctx.moveTo(p1.x, p1.y)
//      ctx.lineTo(p2.x, p2.y)
//    }

    def line(xy1: (Double, Double), xy2: (Double, Double)): Unit = {
      val p1 = tx(Point(xy1._1, xy1._2))
      val p2 = tx(Point(xy2._1, xy2._2))
      ctx.moveTo(p1.x, p1.y)
      ctx.lineTo(p2.x, p2.y)
    }


    def text(string: String, xy: (Double, Double)): Unit = {
      ctx.beginPath()
      ctx.lineWidth = 1
      val p1 = tx(Point(xy._1, xy._2))
      ctx.strokeText(string, p1.x, p1.y)
      ctx.stroke()
    }


    def drawAxes(colorString: String, axesLineWidth: Double): Unit = {

      val arrowHeadLength = 20
      val arrowHeadWidthHalf = 10

      def drawAxis(op: => Unit) = {
        ctx.beginPath()
        ctx.strokeStyle = colorString
        ctx.lineWidth = axesLineWidth
        op
        ctx.stroke()
      }

      def drawXAxisWithArrow() = {
        drawAxis {
          line((minX, 0), (maxX, 0))
          line((maxX, 0), (maxX - arrowHeadLength, arrowHeadWidthHalf))
          line((maxX, 0), (maxX - arrowHeadLength, -arrowHeadWidthHalf))
        }
        text("X", (maxX - 8, -arrowHeadLength))
      }

      def drawYAxisWithArrow() = {
        drawAxis {
          line((0, minY), (0, maxY))
          line((0, maxY), (arrowHeadWidthHalf, maxY - arrowHeadLength))
          line((0, maxY), (-arrowHeadWidthHalf, maxY - arrowHeadLength))
        }
        text("Y", (-arrowHeadLength, maxY -14))
      }


      drawXAxisWithArrow()
      drawYAxisWithArrow()

    }
    def fillBackground(colorString: String): Unit = {
      ctx.fillStyle = colorString
      ctx.fillRect(0, 0, width, height)
    }



    fillBackground("white")
    drawAxes(Color.Black.toString(), 2)


    for (x <- tickInterval to maxX by tickInterval) {
      val isBigTick = (x / tickInterval) % 10 == 0
      val thisTickLength = if (isBigTick) tickLength * 2 else tickLength
      if (isBigTick) {
        text(x.toString, (x - 10, thisTickLength + 5))
        text((-x).toString, (-x - 12, thisTickLength + 5))
      }
      ctx.beginPath()
      ctx.lineWidth = if (isBigTick) 2 else 1
      line((x, -thisTickLength), (x, +thisTickLength))
      line((-x, -thisTickLength), (-x, +thisTickLength))
      ctx.stroke()

      ctx.beginPath()
      ctx.lineWidth = 0.5d
      line((x, minY), (x, maxY))
      line((-x, minY), (-x, maxY))
      ctx.stroke()

    }

    for (y <- tickInterval to height by tickInterval) {
      val isBigTick = (y / tickInterval) % bigTickEvery == 0
      val thisTickLength = if (isBigTick) tickLength * 2 else tickLength
      if (isBigTick) {
        text(y.toString, (thisTickLength + 5, y - 3))
        text((-y).toString, (thisTickLength + 5, -y - 3))
      }
      ctx.beginPath()
      ctx.lineWidth = if ((y / tickInterval) % 10 == 0) 2 else 1
      line((-thisTickLength, y), (thisTickLength, y))
      line((-thisTickLength, -y), (thisTickLength, -y))
      ctx.stroke()

      ctx.beginPath()
      ctx.lineWidth = 0.5d
      line((minX, y), (maxX, y))
      line((minX, -y), (maxX, -y))
      ctx.stroke()

    }


  }


}


