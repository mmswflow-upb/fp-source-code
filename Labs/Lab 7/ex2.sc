

type Img = List[List[Int]]



def show(m: Img): String = {


  def turnRowIntoCharList(l: List[Int]) : String = {
    l.foldRight[String](""){
      case (num, accRowStr) => accRowStr match {
        case "" => num.toString
        case _ => num.toString + "," + accRowStr
      }
    }
  }

  def helperShow(m: Img): String = {
    m.foldRight[String](""){
      case ( row,str) => {
        val rowStr = '\n' + turnRowIntoCharList(row)
        rowStr + str
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




