package genetics.genes

/**
  * Stores a genes variable as a Double in the range 0.0 - 1.0
  */
class Gene(var geneValue: Double) {
  def randomToDouble(): Double = {
    Math.tan((this.geneValue - 0.5) * Math.PI)
  }
  /* You may add methods as needed */

}
