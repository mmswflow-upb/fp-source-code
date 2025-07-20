import java.util

class Dictionary[K,V](inner: List[(K,V)], default: Option[V]) {

  def contains(value: V) : Boolean = {
    inner.exists(pair => pair._2.equals(value))
  }

  def containsKey(key: K): Boolean = {
    inner.exists(pair => pair._1.equals(key))
  }

  def +(newPair: (K,V)): Dictionary[K,V]= {

    new Dictionary[K,V](
      if(!containsKey(newPair._1)) newPair :: inner else inner,
      default
    )
  }

  def get(key: K): Option[V] = {
    inner.find(pair => pair._1.equals(key)) match {
      case None => default
      case Some((k,v)) => Some(v)
    }
  }

  def getOrElse(default: V)(key: K): V = {
    inner.find(pair => pair._1.equals(key)) match {
      case None => default
      case Some((k,v)) => v
    }
  }

  def map(f: (K,V) => (K,V)): Dictionary[K, V] = {
    Dictionary(
      inner.map{
        pair => f(pair._1, pair._2)
      }
    )
  }
  def withDefaultValue(default: V): Dictionary[K,V] = ???
}
object Dictionary {

  def apply[K,V](l: List[(K, V)]): Dictionary[K,V] = {
      new Dictionary[K,V](l, None )
  }

  def apply[K,V](l: List[(K,V)], default: V): Dictionary[K,V] = {
    new Dictionary[K,V](l, Some(default))
  }
}

