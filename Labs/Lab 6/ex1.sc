type Gradebook = List[(String,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook = List(("G",3), ("F", 10), ("M",11), ("P",4))

/*
Go through all elements, add 1 to each and reconstruct the grade book
 */

def increment(g: Gradebook): Gradebook =
  g.map{
    case (name, grade) => if(grade >= 5) (name, grade+1) else (name,grade)
  }

/*
  Apply fold to sum all grades then divide the final result by the number of entries
  */
def average(g: Gradebook): Double =
  g.foldRight[Int](0){
    case ( (name, grade), accGrade ) => accGrade + grade
  }.toDouble/g.size

/*
  partition entries into ones that passed (right side) and ones that failed (left side)
  then take the sizes of the lists and divide by total number of entries to find failing/passing rate
  */
def percentage(g: Gradebook): (Double,Double) =

  if(g.size == 0) (0,0)
  val pair = g.partition((name, grade) => grade < 5)
  (pair._1.size.toDouble/g.size.toDouble, pair._2.size.toDouble/g.size.toDouble)

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
val extgradebook: ExtGradebook = List(("John","FP",4), ("Mario", "SM", 5), ("Mario", "FP", 4), ("Mario", "Physics", 2), ("Ben", "Calculus", 4), ("Ben", "Calculus II", 5), ("Ben", "Numerical Methods", 3), ("Abd", "Calculus", 5))


def atLeastOneFail(g: ExtGradebook): List[Name] =
  g.foldRight[List[Name]](List() : List[Name]){
    case ( (name, lecture, grade), failingStudents) => if(grade < 5 && failingStudents.indexOf(name) == -1) name :: failingStudents else failingStudents
  }

/*
  We compute the key for each element based on the criterion
  */

def groupBy[A, B](l: List[A])(criterion: A => B): List[(B, List[A])] =
  l.foldRight(List.empty[(B, List[A])]) { (elem, acc) =>
    val key = criterion(elem)
    val (matches, others) = acc.partition { case (k, _) => k == key }
    matches match {
      case Nil => (key, List(elem)) :: others
      case (k, group) :: _ => (k, elem :: group) :: others
    }
  }

increment(gradebook)
average(gradebook)
percentage(gradebook)
pass(gradebook)
honorsList(gradebook)

atLeastOneFail(extgradebook)


groupBy[String, Int](List("Mario", "Ana", "Abd", "Omar", "Azzam"))(_.length)
groupBy[(Int, Int), Int](List((1,2), (2,1), (3,3))) (p => p._1 + p._2)