type Gradebook = List[(String,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook = List(("G",3), ("F", 10), ("M",11), ("P",4))

/*
Go through all elements, add 1 to each and reconstruct the grade book if the grade is 5 or higher
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
  Filter out the students who failed, and keep only the names of the students who passed
  */
def pass(g: Gradebook): List[String] =
  g.filter{
    case (name, grade) => grade >= 5
  }.map{
    case (name, grade) => name
  }

/*
  same as above, but this time we also sort them using sortWith, it takes a comparison function if it returns true then the first entry comes before the other one, and vice versa
  so if we compare g1 to g2, and g1 is bigger than g2 then compareTo returns a value bigger than 0 which then returns true to the sortWith function, meaning that the first
  element comes before the second one and vice versa. also when compareTo returns zero it means they're equal so we don't change their order,
  first can come before second or the opposite it doesn't matter they're the same
  */
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


/*
use foldRight to accumulate the list of students who have failed at least once. If any of the names wasn't saved already in the accumulated list then
append the current list to the new element (put the new element as its head) to construct a new list, if it's already in the list we don't change it.
 */

def atLeastOneFail(g: ExtGradebook): List[Name] =
  g.foldRight[List[Name]](List() : List[Name]){
    case ( (name, lecture, grade), failingStudents) => if(grade < 5 && failingStudents.indexOf(name) == -1) name :: failingStudents else failingStudents
  }

/*
  We iterate through the list with foldRight, we compute the key for each element based on the criterion.
  The data type of the keys is B and data type of the elements stored in the lists is A.
  Then we partition the current accumulator (the map) into 2 parts, one that has the same key as the element we've currently reached,
  and the others that have different keys. If the currently computed key isn't in the map already, we add a new pair of (key,value),
  where value is a list containing for now the current element. If the key is already saved in the map,
  then we reconstruct the whole map, by inserting a new (key,value) pair as its head and the other entries that
  have different keys are making up the rest of the map. This new (key,value) pair will have a new value actually, the key stays the same.
  The new value consists of a new list with the current element as the new head of this list, and the other elements that had the same
  key as the current element will make up the rest of the list
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