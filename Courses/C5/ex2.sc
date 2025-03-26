/*
OOP style lists, we won't implement absolutely everything like size and take and drop etc
 */

trait IList {
  def size: Int
  def take(n: Int) : IList
  def drop(n: Int): IList
  def merge(other: IList) : IList
  def mergesort: IList
}

case object Void extends IList {

  override def mergesort: IList = this // this object
  override def merge(other: IList) = other

}

case class Cons(x: Int, xs: IList) extends IList {

  override def merge(other: IList) : Ilist = {
    other match {
      case Cons(y, ys) =>
        if(x < y) Cons(x, xs.merge(other))
    }
  }

  override def mergesort: IList = {
    xs match {
      case Void => this
      case _ => {
        val mid = this.size/2
        this
          .take(mid) // take first half
          .mergesort // sort first half recursively
          .merge(this.drop(mid).mergesort) // merge with second sorted half
      }
    }
  }
}

/*
Locality, operations such as size, take and drop they're local, they only need to know about the current type of object,
but merge is not local, to decide the functionality we need to look at the current object and the other object we're merging with
 */