type Img = List[List[Int]]

object Matrix extends App {

  def show(m: Img): String = {

    def mkString(l: List[Char]) : String = {
      l.foldRight[String](""){
        case (char, accStr) => char + accStr
      }
    }

    def turnRowIntoCharList(l: List[Int]) : List[Char] = {
      l.foldRight[List[Char]](Nil: List[Char]){
        case (num, accL) => accL match {
          case Nil => num.toChar :: accL
          case _ => num.toChar :: ',' :: accL
        }
      }
    }

    def helperShow(m: Img): List[Char] = {
      m.foldRight[List[Char]](Nil: List[Char]){
        case ( row,str) => {
          val rowChars = '\n' :: turnRowIntoCharList(row)
          rowChars.foldRight[List[Char]](Nil: List[Char]){
            case (char,rowAndStr) => char :: rowAndStr
          }
        }
      }
    }

    mkString(helperShow(m))
  }

  // Test Code From Here On
  val m1 = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12))


}

