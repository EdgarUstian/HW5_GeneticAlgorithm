package tests

import org.scalatest._
import genetics.GeneticAlgorithm
import genetics.geometry._

class TestLinearRegression extends FunSuite{
  test("Testing Linear Regression 1"){
    val xValues: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val yValues: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    var xyPoints: List[Point] = List()
    for (i <- xValues.indices){
      xyPoints = xyPoints :+ new Point(xValues(i), yValues(i))
    }
    println(xyPoints)
    val actual = GeneticAlgorithm.linearRegression(xyPoints)
    val expected = new Line(1.0, 0.0)
    println("  actual : ", actual)
    println("expected : ", expected)
    assert(Math.abs(actual.slope - expected.slope) < 0.0255, Math.abs(actual.slope - expected.slope))
    assert(Math.abs(actual.yIntercept - expected.yIntercept) < 0.0255, Math.abs(actual.yIntercept - expected.yIntercept))
  }

  test("Testing Linear Regression 2"){
    val xValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val yValues: List[Double] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    var xyPoints: List[Point] = List()
    for (i <- xValues.indices){
      xyPoints = xyPoints :+ new Point(xValues(i), yValues(i))
    }
    println(xyPoints)
    val actual = GeneticAlgorithm.linearRegression(xyPoints)
    val expected = new Line(-1, 10)
    println("  actual : ", actual)
    println("expected : ", expected)
    assert(Math.abs(actual.slope - expected.slope) < 0.035, Math.abs(actual.slope - expected.slope))
    assert(Math.abs(actual.yIntercept - expected.yIntercept) < 0.155, Math.abs(actual.yIntercept - expected.yIntercept))
  }
}
