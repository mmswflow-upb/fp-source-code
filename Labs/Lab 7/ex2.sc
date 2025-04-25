import scala.annotation.tailrec

type Img = List[List[Int]]



def show(m: Img): String = {



  def helperShow(m: Img): String = {
    m.foldRight[String](""){ // use fold right to keep the order of the rows the same
      case ( row,str) => {
        val rowStr = '\n' + row.mkString(" ") // for each new row we must add a new line character and turn that row into a string
        rowStr + str // concatenate the new row with the rest of the matrix
      }
    }
  }

  helperShow(m)
}

val m1 = List(
  List(1, 2, 3),
  List(4, 5, 6),
  List(7, 8, 9),
  List(10, 11, 12)
)

show(m1)


// We can use fold left to reverse the order of the rows to achieve a vertical flip
// We can do this by using _.reverse or foldLeft
def vFlip(img: Img): Img = img.reverse



val L = List(
  List(1,0,0),
  List(1,0,0),
  List(1,1,1)
)

/*
Here we have the image of the letter L

1 0 0                                      1 1 1
1 0 0 if we flip it vertically and get:    1 0 0
1 1 1                                      1 0 0
 */

show(vFlip(L))

def singleLine(m: Img): List[Int] = {
  m.foldRight[List[Int]](Nil: List[Int])( (row: List[Int], col: List[Int]) => row.head :: col)
}

def remCol(m: Img): Img = {
  m.map((row: List[Int]) => row.drop(1))
}

def transpose(m: Img): Img = {
  if(m.head == Nil) return Nil
  singleLine(m) :: transpose(remCol(m))
}

/*
Here, we test with the letter "j", if we horizontally flip it we get ᒑ (mirrored)

0 0 1 0 0     0 0 1 0 0
0 0 0 0 0     0 0 1 0 0
0 0 1 0 0 =>  0 0 1 0 0
1 0 1 0 0     0 0 1 0 1
1 1 1 0 0     0 0 1 1 1
 */

/*We can implement this either by reversing each row individually or
transpose, vertical flip, transpose again.
*/
def hFlip(img: Img): Img = transpose(vFlip(transpose(img)))

val j = List(
  List(0,0,1,0,0),
  List(0,0,0,0,0),
  List(0,0,1,0,0),
  List(0,0,1,0,0),
  List(1,0,1,0,0),
  List(1,1,1,0,0)
)


show(hFlip(j))

def rot90Right(img: Img): Img = (transpose(img))

show(rot90Right(j))

def rot90Left(img: Img): Img = hFlip(transpose(img))

show(rot90Left(j))

def invert(img: Img): Img = img.map(_.map(255 - _))

show(invert(j))

def cropAt(img: Img, xSt:Int, ySt:Int, xEnd: Int, yEnd: Int): Img = {
  val croppedImg = for {
    (row, rowIndx) <- img.zipWithIndex
    if rowIndx >= ySt && rowIndx <= yEnd
  } yield {
    for {
      (elem, colIndx) <- row.zipWithIndex
      if colIndx >= xSt && colIndx <= xEnd
    } yield elem
  }
  croppedImg
}

val imgToBeCropped = List(List(0,0,1,0,0), List(0,1,0,1,0), List(0,1,1,1,0), List(1,0,0,0,1), List(1,0,0,0,1))

show(imgToBeCropped)
show(cropAt(imgToBeCropped, 1,1,3,2))


def largerPos(img: Img, int: Int): List[(Int,Int)] = {

  for {
    (rowElems, row) <- img.zipWithIndex
    (elem, col) <- rowElems.zipWithIndex
    if elem > int
  } yield (row,col)
}

largerPos(imgToBeCropped, 0)

def contrast(x: Int)(img: Img): Img =
  img.map(_.map((pix: Int) => x + pix))

show(contrast(10)(imgToBeCropped))

def hglue(img1: Img, img2: Img): Img = {

  def auxHGlue(row1: List[Int], row2: List[Int]): List[Int] = {
    row1 match {
      case Nil => row2
      case x :: restRow1 => x :: auxHGlue(restRow1, row2)
    }
  }

  img1.zip(img2).foldRight[Img](Nil){
    case ((row1,row2), newImg) => auxHGlue(row1,row2) :: newImg
  }
}

show(hglue(j,j))

def vglue(img1: Img, img2: Img): Img = {
  def aux(firstImg: Img): Img =
    firstImg match {
      case Nil => img2
      case row :: restImg1 => row :: aux(restImg1)
    }
  aux(img1)
}



show(vglue(j, j))


def drawDiagonals(img: Img): Img = {
  val size = img.length

  (0 until size).toList.map { i =>
    (0 until size).toList.map { j =>
      if (j == i || j == (size - 1 - i)) 1 else img(i)(j)
    }
  }
}

show(drawDiagonals(L))
