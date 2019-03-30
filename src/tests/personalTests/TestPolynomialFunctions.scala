package tests.personalTests

import genetics.GeneticAlgorithm
import org.scalatest._
import genetics.genes._
import genetics.geometry._

class TestPolynomialFunctions extends FunSuite{

  test("Using Polynomial") {
    def samplesize(degree: Int): List[Gene] = {
      var listGenes: List[Gene] = List()
      for (i <- 0 to degree){
        listGenes = listGenes :+ new Gene(0)
      }
      listGenes
    }

    var listAnimals: List[Animal] = List()
    def converter: List[Gene] => Polynomial = GeneticAlgorithm.geneToPolynomial()

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

    animalMaker(samplesize(2))


    //Turns every creature into object in solution space
    //Make global for clearing reasons
    var mapSolution: Map[Animal, Polynomial] = Map()

    def animalToSolution(animals: List[Animal]) {
      for (animal <- animals) {
        val singleSolution: Polynomial = converter(animal.genes)
        mapSolution = mapSolution + (animal -> singleSolution)
      }
    }

    //Create the first Generation fo solutions
    animalToSolution(listAnimals)

    val xValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 8)
    val yValues: List[Double] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    var xyPoints: List[Point] = List()
    for (i <- xValues.indices) {
      xyPoints = xyPoints :+ new Point(xValues(i), yValues(i))
    }

    def fitness = GeneticAlgorithm.polyFitFuncGen(xyPoints)

    def bestFitnessFinder(mapSolutions: Map[Animal, Polynomial]): Unit = {
      for ((animal, polynomial) <- mapSolutions) {
        animal.fitness = fitness(polynomial)
        //println(animal.fitness)
      }
    }

    bestFitnessFinder(mapSolution)

    //Sets up comparator by Animal's fitness to be used by the sorter
    def compareFitness(a1: Animal, a2: Animal): Boolean = {
      a1.fitness > a2.fitness
    }

    //Sort the creature fitness based on the function made above
    def sorter(): List[Double] = {
      var sortedFitness: List[Double] = List()
      val animalsSorted: List[Animal] = listAnimals.sortWith(compareFitness)
      for (animal <- animalsSorted){
        sortedFitness = sortedFitness :+ animal.fitness
      }
      sortedFitness
    }

    //println(sorter())

    //Clears the map as it is not needed for now
    def clear(): Unit = {
      for ((animal, solution) <- mapSolution) {
        mapSolution = mapSolution - animal
      }
    }
  }

  test("Testing Polynomial Fitness Function"){
    val xValues: List[Double] = List(-3, -2, -1, 0, 1, 2, 3)
    val yValues: List[Double] = List(-9, -4, -1, 0, 1, 4, 9)
    var xyPoints: List[Point] = List()
    for (i <- xValues.indices){
      xyPoints = xyPoints :+ new Point(xValues(i), yValues(i))
    }
    def fitness = GeneticAlgorithm.polyFitFuncGen(xyPoints)
    println(fitness(new Polynomial(List(0, 0, 1))))
  }
}
