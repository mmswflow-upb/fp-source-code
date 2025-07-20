import scala.collection.immutable.TreeMap

/*
VERY IMPORTANT: SCALA MAPS DONT ALLOW DUPLICATES!!
 */

case class Term(pair: (Int,Int)) {

  def +(t2: Term): Term = {
    Term(this.pair._1, this.pair._2 + t2.pair._2)
  }
  def *(t2: Term): Term = {
    Term((this.pair._1 + t2.pair._1, this.pair._2 * t2.pair._2))
  }

  override def toString: String = {
    this.pair._2 + "x^" + this.pair._1
  }
}



case class Polynomial (nonZeroTerms: TreeMap[Int,Int]) {
  def *(n: Int): Polynomial = {
    Polynomial(
      nonZeroTerms.map(pair => (pair._1,pair._2 * n))
    )
  }

  override def toString: String = {

    nonZeroTerms.reduce((t1, t2) => t1.toString() + " " + t2.toString())
  }

  def hasRoot(r: Int): Boolean = nonZeroTerms.map(t => Math.pow(r, t._1) * t._2).sum == 0.0

  def +(p2: Polynomial): Polynomial = {

      Polynomial(
        nonZeroTerms
          .zip(p2.nonZeroTerms.toList)
          .toList
          .map{
            pair => new Term(pair._1) + new Term(pair._2)
          }
      )
  }

  def *(p2: Polynomial): Polynomial = {
    Polynomial(
      for {
        (exp1, coef1) <- this.nonZeroTerms.toList
        (exp2, coef2) <- p2.nonZeroTerms.toList
      } yield Term(exp1 + exp2, coef1 * coef2)
    )
  }
}

object Polynomial {

  def normalizeTerms(ts: List[Term]): TreeMap[Int,Int]= {
    TreeMap(ts
      .groupBy(t => t.pair._1)
      .map{
        case (power, ts) => (power, ts.map(t => t.pair._2).sum)
      }
      .toSeq: _*
    )

  }

  def apply(ts: List[Term]): Polynomial = {
    new Polynomial(normalizeTerms(ts))
  }
}