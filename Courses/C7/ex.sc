type Str = List[Char]

type Tabular[A] = List[List[A]] // Tabular structure

// Parametrize readTabular with A
def readTabular[A](read: Str => A)(s: String) : Tabular[A] = {
  def split(delim: Char)(content: Str): List[Str] = {

    def op(c: Char, acc: List[Str]): List[Str] = {
      acc match {
        case Nil => if (c == delim) Nil else List(List(c)) // first character in the sequence that is not a delim
        case block :: xs => if (c == delim) Nil :: acc else (c :: block) :: xs
      }
    }

    content.foldRight(Nil: List[Str])(op)
  }
  split('\n')(s.toList)
    .map(split(','))
    .map((l: List[Str]) => l.map((cell: Str) => read(cell)) )// use underscores instead of lambdas for simplicity

}

/*
We're using the infix notation on matrices
this is not overloading what we're doing here because
we don't have multiple definitions of the same function multiple times, also
it's not overriding because we're not inheriting from another class
 */

class Matrix(val inner: Tabular[Int]) {
  def scalar(value: Int): Matrix = new Matrix (inner.map(_.map(_*value) ))
  def +(other: Matrix): Matrix = {

    Matrix(
    this.inner
      .zip(other.inner)
      .map(p => p._1.zip(p._2))
      .map(_.map(p => p._1 + p._2))
    )
  }

  def transpose: Matrix = {
    def aux_transpose(t: Tabular[Int]): Tabular[Int] =
      t match {
        case Nil :: _ => Nil
        case _ => t.map(_.head) :: aux_transpose (t.map (_.tail) ) //first line of the transposition "cons" the recursive transposition of the rest of the matrix
      }

    new Matrix(aux_transpose(inner))
  }

  def *(other: Matrix): Matrix = {
    val transM2 = other.transpose

    new Matrix(this.inner.map((row1: List[Int]) => {
      transM2.inner.map((row2: List[Int]) => {
        row1.zip(row2).map((pair: (Int, Int)) => pair._1 * pair._2).sum
      })
    }))
  }

  /*
    for li <- this.inner
       yield
          for cj <- other.transpose.inner
             yield
                (for p <- li.zip(cj)
                   yield p._1 * p._2).foldRight(0)(_+_)
  */

  override def toString = ???


}

/*
if f is a member function, part of a class C and object o is an instance of C, then instead of
o.f(v), in Scala we can write o f v
 */


/*
For expressions

 we have generators: x <- List()
 */

for (x <- List(1,2,3,4)) yield x + 1 // adds 1 to each element

// This expression is not a for, its syntactic sugar for a map
for ( x <- List(1,2,3,4) if x % 2 == 0) yield x * 2

for x <- List(1,2,3)
    y <- List(4,5,6)
yield (x,y) // a zip


/*
Question:

If I want to create the matrix:

List(
  List( (1,4), (2,4), (3,4) ),
  List( (1,5), (2,5), (3,5) ),
  List( (1,6), (2,6), (3,6) )
 )
 */

val l1 = List(1,2,3)
val l2 = List(4,5,6)

for x <- l1
  yield
    for y <- l2
      yield (x,y)


/*
As we need to create matrices from other datatypes, the project becomes polluted by "from...(...): Matrix"
and we need to keep track of all these function names, and where to use them, this is what we're trying to solve
 */

class F {
  def apply(x: Int): Int = x + 1
  def apply(x: Int, y: Int) : Int = x +y


}


/*
  The method apply is a special method, it allows us to "call" objects. More concretely:
   */

val o = new F
o.apply(1)
o(1)
o(1,2)
/*
Don't forget that everything is an object in Scala, even functions, especially built-in functions
and they support the apply method. When we define the apply function in a class, we can "call" the object,
but actually we're calling the function from inside the class, we can also have multiple apply methods
with overloading
 */


class MatrixCreator {
  def apply(s: String): Matrix = {
    val parse = readTabular(_.foldRight("")(_+_).toInt)
    new Matrix(parse(s))
  }

  def apply(t: Tabular[Int]): Matrix = new Matrix(t)
}

val creator = new MatrixCreator
creator("1,2\n3,4")
creator(List(List(1,2), List(3,4)))

/*
But still this isn't very nice, do we really have to instantiate a creator object each time?
Since we only need a single instance of creator, we can make it an object
 */


object MatrixCreator {
  def apply(s: String): Matrix = {
    val parse = readTabular(_.foldRight("")(_+_).toInt)
    new Matrix(parse(s))
  }

  def apply(t: Tabular[Int]): Matrix = new Matrix(t)
}

MatrixCreator("1,2\n 3,4")
MatrixCreator(List(List(1,2), List(3,4)))

/*
This is still not nice, we can improve it further
We can create something called a companion object, which is an object with the same name as the
class name, where we also implement the apply functions, so we don't have to call a separate
class anymore
 */

object Matrix {
  def apply(s: String): Matrix = {
    val parse = readTabular(_.foldRight("")(_+_).toInt)
    new Matrix(parse(s))
  }

  def apply(t: Tabular[Int]): Matrix = new Matrix(t)
}

Matrix("1,2\n 3,4")
Matrix(List(List(1,2), List(3,4)))

