/*
Let us build a more complex example, using our knowledge

 */

// multi-line strings in scala
val m =
  """
    |1,2,3
    |4,5,6
    |7,8,9
    |""".stripMargin
// m: String
// parse(m) : List[List[Int]], we need to parse the string into a matrix of ints
// we will have '1', ',' ,'2', ',', '3', '\n' so we need to separate them based on the new line to split them into rows
type Str = List[Char]
val flat_m : Str = m.toList

def parse(s: String) : List[List[Int]] = {
  def split(delim: Char)(content: Str): List[Str] = {

    def op(c: Char, acc: List[Str]): List[Str] = {
      acc match {
        case Nil => if (c == delim) Nil else List(List(c)) // first character in the sequence that is not a delim
        case block :: xs => if (c == delim) Nil :: acc else (c :: block) :: xs
      }
    }

    content.foldRight(Nil: List[Str])(op)
  }
  def makeInt(s: Str): Int = s.foldRight("")(_+_).toInt
  split('\n')(s.toList)
    .map(split(','))
    .map((l: List[Str]) => l.map((cell: Str) => makeInt(cell)) )// use underscores instead of lambdas for simplicity

}
List('1', '2', '3', '4').foldRight("")(_ + _).toInt

val matrix = parse(m)


/*
Now the goal is to create a generalized parse function, that will create a table  of any type
 */
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
val matrix2 = readTabular[Int](_.foldRight("")(_+_).toInt)(m)
