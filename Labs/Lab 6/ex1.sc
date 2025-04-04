type Gradebook = List[(String,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook = List(("G",3), ("F", 10), ("M",6), ("P",4))

/*
Go through all elements, add 1 to each and reconstruct the grade book
 */

def increment(g: Gradebook): Gradebook =
  g.map{
    case (name, grade) => (name, grade+1)
  }

/*
  Apply fold to sum all grades then divide the final result by the number of entries
  */
def average(g: Gradebook): Double =
  g.foldRight[Int](0){
    case ( (name, grade), accGrade ) => accGrade + grade
  }/g.size

/*
  partition entries into ones that passed (right side) and ones that failed (left side)
  then take the sizes of the lists and divide by total number of entries to find failing/passing rate
  */
def percentage(g: Gradebook): (Double,Double) =
  val pair = g.partition((name, grade) => grade < 5)
  (pair._1.size/g.size, pair._2.size/g.size)

/*

  */
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


def atLeastOneFail(g: ExtGradebook): List[Name] =
  g.foldRight[List[Name]](List() : List[Name]){
    case ( (name, lecture, grade), failingStudents) => if(grade < 5 && failingStudents.indexOf(name) == -1) name :: failingStudents else failingStudents
  }

def groupBy[A,B](l: List[A])(criterion: A => B): List[(B,List[A])] = ???



