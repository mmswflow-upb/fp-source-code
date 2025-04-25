type Matrix = List[List[Int]]

val m1 = List(List(1,1,1), List(1,1,1), List(1,1,1))
val m2 = List(List(1,0,0), List(0,1,0), List(0,0,1))
val m3 = List(List(1,2,3), List(4,5,6), List(7,8,9), List(10,11,12))
val m4 = List(List(1,2,3), List(4,5,6), List(7,8,9))
val m5 = List(List(1,2,3,4), List(5,6,7,8), List(9,10,11,12))
val m6 = List(List(1,2,3), List(4,5,6), List(7,8,9), List(10,11,12))
def sum(m: Matrix): Int = m.foldRight[Int](0)((row: List[Int], accSum: Int) => accSum + row.sum)

def scalarMult(const: Int, m: Matrix): Matrix =
  m.map(_.map((x: Int) => x * const))

def add(m1: Matrix, m2: Matrix): Matrix = {

  m1.zip(m2).foldRight[Matrix](Nil: Matrix){
    case ((row1, row2),accM)  =>
      row1.zip(row2).foldRight[List[Int]](Nil: List[Int]){
        case ((num1, num2),accRow)  => (num1+num2) :: accRow
      }:: accM
  }
}



def singleLine(m: Matrix): List[Int] = {
  m.foldRight[List[Int]](Nil: List[Int])( (row: List[Int], col: List[Int]) => row.head :: col)
}

def remCol(m: Matrix): Matrix = {
  m.map((row: List[Int]) => row.drop(1))
}

def printMatrix(m: Matrix): Unit = {
  m.foreach((row: List[Int]) => println(row))
}

def transpose(m: Matrix): Matrix = {
  if(m.head == Nil) return Nil
  singleLine(m) :: transpose(remCol(m))
}



sum(m2) // 3
sum(m1) // 9
sum(m3) // 78

printMatrix(scalarMult(5, m1))
printMatrix(scalarMult(-1, m3))

printMatrix(add(m1,m2))

singleLine(m2)

printMatrix(remCol(m2))

printMatrix(m3)
printMatrix(transpose(m3))


def mult(m1: Matrix, m2: Matrix): Matrix = {
  val transM2 = transpose(m2)

  m1.map((row1: List[Int]) => {
    transM2.map((row2: List[Int]) => {
      row1.zip(row2).map((pair: (Int, Int)) => pair._1 * pair._2).sum
    })
  })
}


printMatrix(mult(m1,m4))

printMatrix(transpose(m5))
printMatrix(transpose(m6))

printMatrix(mult(m5,m6))