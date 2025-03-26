trait OList{
  def head: Int
  def tail: OList
  def foldRight[B](acc: B)(op: (Int,B) => B): B
  def foldLeft[B](acc: B)(op: (B,Int) => B): B
  def indexOf(i: Int): Int
  def filter(p: Int => Boolean): OList
  def map(f: Int => Int): OList
  def partition(p: Int => Boolean): (OList, OList)
  def slice(start: Int, stop: Int): OList
  def forall(p: Int => Boolean): Boolean
}