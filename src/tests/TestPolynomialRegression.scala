package tests

import org.scalatest._
import genetics.GeneticAlgorithm
import genetics.geometry._

class TestPolynomialRegression extends FunSuite{
  test("Testing Linear Regression 1"){
    val myLine: Polynomial = new Polynomial(List(1, 0, -1))
    var linePlot: List[Point] = List()
    for (i <- -5 to 5){
      linePlot = linePlot :+ new Point(i, myLine.evaluate(i))
    }
    println(linePlot)
    val actual = GeneticAlgorithm.polynomialRegression(linePlot, 2)
    println("expected : ", myLine)
    println("  actual : ", actual)
    assert(Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head) < 0.0555, Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head))
    assert(Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)) < 0.0555, Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)))
    assert(Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)) < 0.1777, Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)))
  }

  test("Testing Linear Regression 2"){
    val myLine: Polynomial = new Polynomial(List(0, 0, -1))
    var linePlot: List[Point] = List()
    for (i <- -5 to 5){
      linePlot = linePlot :+ new Point(i, myLine.evaluate(i))
    }
    println(linePlot)
    val actual = GeneticAlgorithm.polynomialRegression(linePlot, 2)
    println("expected : ", myLine)
    println("  actual : ", actual)
    assert(Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head) < 0.0555, Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head))
    assert(Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)) < 0.0555, Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)))
    assert(Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)) < 0.1, Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)))
  }

  test("Testing Line as Polynomial"){
    val myLine: Polynomial = new Polynomial(List(0, 1, 1))
    var linePlot: List[Point] = List()
    for (i <- -5 to 5){
      linePlot = linePlot :+ new Point(i, myLine.evaluate(i))
    }
    println(linePlot)
    val actual = GeneticAlgorithm.polynomialRegression(linePlot, 2)
    println("expected : ", myLine)
    println("  actual : ", actual)
    assert(Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head) < 0.0555, Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head))
    assert(Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)) < 0.555, Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)))
    assert(Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)) < 0.1555, Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)))
  }

  test("Test Making Polynomial") {
    val points: List[Point] = List(
      new Point(1, 9),
      new Point(2, 1),
      new Point(3, 7),
      new Point(4, 6),
      new Point(5, -5),
      new Point(6, -7)
    )
    val myLine: Polynomial = new Polynomial(List(5.4000000000000314e+000, 1.9214285714285482e+000, -6.7857142857142527e-001))
    var linePlot: List[Point] = List()
    for (i <- -5 to 5) {
      linePlot = linePlot :+ new Point(i, myLine.evaluate(i))
    }
    println(linePlot)
    val actual: Polynomial = GeneticAlgorithm.polynomialRegression(points, 2)
    println("expected : ", myLine)
    println("  actual : ", actual)
    assert(Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head) < 0.1, Math.abs(actual.coefficients.reverse.head - myLine.coefficients.reverse.head))
    assert(Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)) < 0.5, Math.abs(actual.coefficients.reverse(1) - myLine.coefficients.reverse(1)))
    assert(Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)) < 3, Math.abs(actual.coefficients.reverse(2) - myLine.coefficients.reverse(2)))
  }
}
