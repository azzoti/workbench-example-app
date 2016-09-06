package org.lazyluke

import org.scalajs.dom
import org.scalajs.dom.html

import scala.util.control.NonFatal
import scalatags.JsDom.all._


object FormulaGui {

  private val formulaInput = input(`type` := "text", placeholder := "sin(x * 0.01745)", value := "sin(x * 0.01745)", size := "20").render

  private val formulaError = span(color := "red").render

  private val xStartError = span(color := "red").render
  private val xEndError = span(color := "red").render
  private val xStepError = span(color := "red").render

  private val xStart = input(`type` := "text", placeholder := "-360", value := "-360", size := "4").render
  private val xEnd = input(`type` := "text", placeholder := "+360", value := "+360", size := "4").render
  private val xStep = input(`type` := "text", placeholder := "+1", value := "+1", size := "2").render


  def keyupHandler(inputChecker: () => Option[Double])(ev: dom.raw.UIEvent): Unit = {
    inputChecker()
  }

  def renderFormulaHtml(target: html.Div, formulaKeyUpHandler: (dom.raw.UIEvent) => Unit): Unit = {

    formulaInput.onkeyup = formulaKeyUpHandler

    target.appendChild(
      div(
        span("Graph of y = "),
        formulaInput, formulaError,
        span(" start with x = "), xStart, xStartError,
        span(" end with x = "), xEnd, xEndError,
        span(" in steps of "), xStep, xStepError
      ).render
    )

    xStart.onkeyup = keyupHandler(xStartValue) _
    xEnd.onkeyup = keyupHandler(xEndValue) _
    xStep.onkeyup = keyupHandler(xStepValue) _

  }

  def formula() = formulaInput.value

  def xStartValue() = getOptionDoubleWithErrorCheck(xStart, xStartError)
  def xEndValue() = getOptionDoubleWithErrorCheck(xEnd, xEndError)
  def xStepValue() = getOptionDoubleWithErrorCheck(xStep, xStepError)

  private def getOptionDoubleWithErrorCheck(s: html.Input, errorSpan: html.Span) = toOptionDouble(s.value) match {
    case a@None =>
      errorSpan.textContent = "?"
      a
    case d@Some(_) =>
      errorSpan.textContent = ""
      d
  }

  def toOptionDouble(s: String): Option[Double] = {
    try {
      Some(s.toDouble)
    } catch {
      case NonFatal(_) => None
    }
  }

  def showError() {
    formulaError.textContent = "???"
  }

  def clearError() {
    formulaError.textContent = ""
  }


}
