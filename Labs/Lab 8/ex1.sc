trait Nat {
  def +(other: Nat): Nat
}
case object Zero extends Nat{
  override def +(other: Nat): Nat = other
}
case class Succ(n: Nat) extends Nat{
  override def +(other: Nat): Nat = Succ(n + other)
}

object Nat { // a companion object for trait Nat
  def apply(i: Int): Nat = if(i > 0) Succ(apply(i-1)) else Zero
  def apply(i: String): Nat = this.apply(i.toInt)

  def fromList(l: List[Integer]) : List[Option[Nat]] = l.map(i => if(i >= 0) Some(this.apply(i)) else None)

  def fromOptions(l: List[Option[Nat]]): Option[List[Nat]] = {
    if(l.contains(None)) None else Some(l.map { case Some(i) => i })
  }
}