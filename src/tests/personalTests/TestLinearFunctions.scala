package tests.personalTests

import genetics.GeneticAlgorithm
import org.scalatest._
import genetics.genes._
import genetics.geometry._

class TestLinearFunctions extends FunSuite{

  test("Using Linear"){
    var listAnimals: List[Animal] = List()
    def converter: List[Gene] => Line = GeneticAlgorithm.geneToLine()

    //Creates 20 random Animals
    def animalMaker(sample: List[Gene]): List[Animal] = {
      for (i <- 1 to 20) {
        var chromosome: List[Gene] = List()
        for (genie <- sample) {
          val gene: Gene = new Gene(Math.random())
          chromosome = chromosome :+ gene
        }
        val creature: Animal = new Animal(chromosome)
        //println(creature)
        listAnimals = listAnimals :+ creature
      }
      listAnimals
    }
    animalMaker(List(new Gene(0), new Gene(0)))

    //Turns every creature into object in solution space
    //Make global for clearing reasons
    var mapSolution: Map[Animal, Line] = Map()
    def animalToSolution(animals: List[Animal]) {
      for (animal <- animals) {
        val singleSolution: Line = converter(animal.genes)
        mapSolution = mapSolution + (animal -> singleSolution)
      }
    }

    //Create the first Generation fo solutions
    animalToSolution(listAnimals)

    val xValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 8)
    val yValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    var xyPoints: List[Point] = List()
    for (i <- xValues.indices){
      xyPoints = xyPoints :+ new Point(xValues(i), yValues(i))
    }

    def fitness = GeneticAlgorithm.lineFitFuncGen(xyPoints)

    def bestFitnessFinder(mapSolutions: Map[Animal, Line]): Unit = {
      for ((animal, line) <- mapSolutions) {
        animal.fitness = fitness(line)
      }
    }

    bestFitnessFinder(mapSolution)

    //Sets up comparator by Animal's fitness to be used by the sorter
    def compareFitness(a1: Animal, a2: Animal): Boolean = {
      a1.fitness > a2.fitness
    }

    //Sort the creature fitness based on the function made above
    val animalsSorted: List[Animal] = listAnimals.sortWith(compareFitness)

    for (animal <- animalsSorted){
      println(animal.fitness, mapSolution(animal))
    }

    //Clears the map as it is not needed for now
    for ((animal, solution) <- mapSolution){
      mapSolution = mapSolution - animal
    }
  }

  test("Testing Linear Fitness Function"){
    val xValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 8)
    val yValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    var xyPoints: List[Point] = List()
    for (i <- xValues.indices){
      xyPoints = xyPoints :+ new Point(xValues(i), yValues(i))
    }
    def fitness = GeneticAlgorithm.lineFitFuncGen(xyPoints)
    println(fitness(new Line(0.9455, 0.1455)))
    println(fitness(new Line(1.003, -0.010)))
  }

}
