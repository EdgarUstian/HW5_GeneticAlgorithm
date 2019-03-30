package genetics

import genetics.genes.{Animal, Gene}
import genetics.geometry._

object GeneticAlgorithm {
  def geneticAlgorithm[T](fitnessFunction: T => Double, converter: List[Gene] => T, sample: List[Gene]): T = {
    //Creates 20 random Animals
    var listAnimals: List[Animal] = List()
    for (i <- 1 to 70){
      var chromosome: List[Gene] = List()
      for (genie <- sample){
        val gene: Gene = new Gene(Math.random())
        chromosome = chromosome :+ gene
      }
      val creature: Animal = new Animal(chromosome)
      listAnimals = listAnimals :+ creature
    }

    //Turns every creature into object in solution space
      //Make global for clearing reasons
      var mapSolutions: Map[Animal, T] = Map()
    def animalToSolution(animals: List[Animal]): Unit = {
      for (animal <- animals) {
        val singleSolution: T = converter(animal.genes)
        mapSolutions = mapSolutions + (animal -> singleSolution)
      }
    }

    //Create the first Generation for solutions
    animalToSolution(listAnimals)

    //Sets up comparator by Animal's fitness to be used by the sorter
    def compareFitness(a1: Animal, a2: Animal): Boolean = {
      a1.fitness > a2.fitness
    }

    //Create a mutated form of Animal provided
    def mutator(animal: Animal): Animal = {
      var mutGenes: List[Gene] = List()
      for (gene <- animal.genes){
        val mutGene: Gene = new Gene(gene.geneValue + (-1 + Math.random() * 2) * (1/100))
        if (mutGene.geneValue > 1.0){
          mutator(animal)
        }
        else if (mutGene.geneValue < 0.0){
          mutator(animal)
        }
        mutGenes = mutGenes :+ mutGene
      }
      new Animal(mutGenes)
    }

    //Creates offspring by combination of 2 Animals
    def combiner(animal1: Animal, animal2: Animal): Animal = {
      var combinedGenes: List[Gene] = List()
      if (animal1.genes.size == animal2.genes.size){
        for (i <- animal1.genes.indices){
          val average: Double = (animal1.genes(i).geneValue + animal2.genes(i).geneValue)/2
          combinedGenes = combinedGenes :+ new Gene(average)
        }
      }
      new Animal(combinedGenes)
    }

    //Create a list for the nextGeneration to inhabit
      //Populate it with the first Generation
    var nextGen: List[Animal] = listAnimals

    //Start and End of Generations
    var start: Int = 0
    val end: Int = 7000
    while (start < end){
      //Runs FitnessFunction on every one of the creatures' solutions
        //Goes over every key(Animal) and value(respective solution)
      for ((animal, line) <- mapSolutions) {
        animal.fitness = fitnessFunction(line)
      }

      //Clears the map as it is not needed for now
      for ((animal, solution) <- mapSolutions){
        mapSolutions = mapSolutions - animal
      }

      //Sort the creature fitness based on the function made above
      val animalsSorted: List[Animal] = nextGen.sortWith(compareFitness)
        //Empty out the sorted List
        nextGen = List()

      //Define the variables for the next generation
        //Best fit and their mutations
        val mostFit: Animal = animalsSorted.head //A
          val mostFitMut1: Animal = mutator(animalsSorted.head) //C
          val mostFitMut2: Animal = mutator(animalsSorted.head) //D
        val secondFitMut: Animal = mutator(animalsSorted(1)) //B
        //Combined offspring
        val AB: Animal = combiner(mostFit, secondFitMut)
        val AC: Animal = combiner(mostFit, mostFitMut1)
        val AD: Animal = combiner(mostFit, mostFitMut2)
        val BC: Animal = combiner(secondFitMut, mostFitMut1)
        val BD: Animal = combiner(secondFitMut, mostFitMut2)
        val CD: Animal = combiner(mostFitMut1, mostFitMut2)

      //Make list of of these 10 Animals
        //FirstHalf of pre-made Animals
        val firstHalf: List[Animal] = List(
          mostFit, secondFitMut, mostFitMut1, mostFitMut2,
          AB, AC, AD, BC, BD, CD
        )

        //SecondHalf of random Animals
        var secondHalf: List[Animal] = List()
      for (i <- 1 to 60){
        var chromosome: List[Gene] = List()
        for (genie <- sample){
          val gene: Gene = new Gene(Math.random())
          chromosome = chromosome :+ gene
        }
        val creature: Animal = new Animal(chromosome)
        secondHalf = secondHalf :+ creature
      }

      //Combine twoHalves to make nextGen
      nextGen = firstHalf ::: secondHalf

      //Create another set of solutions based on this new Generation
      animalToSolution(nextGen)

      //Update Generation Count
      start += 1

//      println(nextGen.head.fitness, start)
    }

    //Return the bestFit solution
    val bestFit: T = converter(nextGen.head.genes)
    println(nextGen.head.fitness)
    bestFit
  }

  //Converts List[Gene] -> Line
  def geneToLine(): List[Gene] => Line = {
    genes: List[Gene] => {
      val slope: Double = genes.head.randomToDouble()
      val yIntercept: Double = genes(1).randomToDouble()
      new Line(slope, yIntercept)
    }
  }

   //A way to provide fitness of a given Line to a List[Point]
  def lineFitFuncGen(points: List[Point]): Line => Double = {
    //Sum of the line given
    line: Line => {
      var fitness: Double = 0.0
      var lineSum: Double = 0.0
      for (point <- points) {
        lineSum += Math.abs(line.evaluate(point.x) - point.y)
      }
      fitness = 1.0 - (Math.atan(lineSum)/Math.PI)
      fitness
    }
  }

  //Performs geneticAlgorithm to find LinearRegression
  def linearRegression(points: List[Point]): Line = {
    val sampleList: List[Gene] = List(new Gene(0), new Gene(0))
    def lineToDouble = lineFitFuncGen(points)
    geneticAlgorithm[Line](lineToDouble, geneToLine(), sampleList)
  }


  //Converts List[Gene] -> Polynomial
  def geneToPolynomial(): List[Gene] => Polynomial = {
    genes: List[Gene] => {
      var coefficients: List[Double] = List()
      for (gene <- genes){
        coefficients = coefficients :+ gene.randomToDouble()
      }
      new Polynomial(coefficients)
    }
  }

  //A way to provide fitness of a given Polynomial to a List[Point]
  def polyFitFuncGen(points: List[Point]): Polynomial => Double = {
    //Sum of the line given
    polynomial: Polynomial => {
      var fitness: Double = 0.0
      var lineSum: Double = 0.0
      for (point <- points) {
        lineSum += Math.abs(polynomial.evaluate(point.x) - point.y)
      }
      fitness = 1.0 - (Math.atan(lineSum)/Math.PI)
      fitness
    }
  }

  //Performs geneticAlgorithm to find PolynomialRegression
  def polynomialRegression(points: List[Point], degree: Int): Polynomial = {
    def samplesize(): List[Gene] = {
      var listGenes: List[Gene] = List()
      for (i <- 0 to degree){
        listGenes = listGenes :+ new Gene(0)
      }
      listGenes
    }
    def polyToDouble = polyFitFuncGen(points)
    geneticAlgorithm[Polynomial](polyToDouble, geneToPolynomial(), samplesize())
  }
}
