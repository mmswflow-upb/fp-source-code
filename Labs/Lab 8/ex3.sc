case class Polynomial (nonZeroTerms: Map[Int,Int]) {
  def *(n: Int): Polynomial = {
    new Polynomial(
      nonZeroTerms.map(pair => (pair._1,pair._2 * n))
    )
  }

  override def toString: String = {
    def termToString(term: (Int, Int)): String = {
      term._2+ "x^" +  term._1
    }
    nonZeroTerms.toStream.sortBy(pair => pair._1).reduce((pair1, pair2) => termToString(pair1) + " " + termToString(pair2))
  }

  def hasRoot(r: Int): Boolean = nonZeroTerms.map(term => Math.pow(r, term._1) * term._2).sum == 0.0

  def +(p2: Polynomial): Polynomial = {

  }
}