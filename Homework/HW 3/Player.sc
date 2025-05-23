trait Player {
  def complement: Player

  def toString: String
}

case object One extends Player {
  override def complement: Player = Two

  override def toString: String = "X"
}
case object Two extends Player {
  override def complement: Player = One

  override def toString: String = "0"
}
case object Empty extends Player {
  override def complement: Player = Empty
  override def toString: String = "."
}

