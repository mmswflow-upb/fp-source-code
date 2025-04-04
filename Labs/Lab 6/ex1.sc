type Gradebook = List[(String,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook = List(("G",3), ("F", 10), ("M",6), ("P",4))

def increment(g: Gradebook): Gradebook =
  g.map{
    case (name, grade) => (name, grade+1)
  }

def average(g: Gradebook): Double =
  g.foldRight[Int](0){
    case ( (name, grade), accGrade ) => accGrade + grade
  }/g.size

def percentage(g: Gradebook): (Double,Double) =
  val pair = g.partition((name, grade) => grade < 5)
  (pair._1.size/g.size, pair._2.size/g.size)

def pass(g: Gradebook): List[String] =
  g.filter{
    case (name, grade) => grade >= 5
  }.map{
    case (name, grade) => name
  }


def honorsList(g: Gradebook): List[String] =
  g.filter{
    case (name, grade) => grade >= 5
  }.sortWith{
    case ((n1, g1), (n2,g2)) => g1.compareTo(g2) >= 0
  }.map{
    case (name, grade) => name
  }


/*
  Extended Grade book exercises
 */

type Name = String
type Lecture = String
type ExtGradebook = List[(Name,Lecture,Int)]
val egradebook: ExtGradebook = List(("John","FP",4))

//def atLeastOneFail(g: ExtGradebook): List[Name] =



