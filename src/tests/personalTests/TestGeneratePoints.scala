package tests.personalTests

import genetics.GeneticAlgorithm
import org.scalatest._
import genetics.genes._
import genetics.geometry._

class TestGeneratePoints extends FunSuite{
  test("Test Making Line"){
    val myLine: Line = new Line(1, 0)
    var linePlot: List[Point] = List()
    for (i <- 1 to 10){
      linePlot = linePlot :+ new Point(i, myLine.evaluate(i))
    }
    println(linePlot)
  }

  test("Test Making Polynomial"){
    val points: List[Point] = List(
      new Point(1, 9),
      new Point(2, 1),
      new Point(3, 7),
      new Point(4, 6),
      new Point(5, -5),
      new Point(6, -7)
    )
    val myLine: Polynomial = new Polynomial(List(0, 0, 1))
    var linePlot: List[Point] = List()
    for (i <- -5 to 5){
      linePlot = linePlot :+ new Point(i, myLine.evaluate(i))
    }
    println(linePlot)
    println(GeneticAlgorithm.polynomialRegression(points, 2))
  }
}