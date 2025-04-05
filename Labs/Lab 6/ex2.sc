type Str = List[Char]
type Email = Str

val email1 = "ana@aol.com".toList
val email2 = "ana@aol.ro".toList
val email3 = "jim@cx.com".toList
val email4 = "mary@mail.com".toList
val email5 = "mary@mail.ro".toList
val email6 = "mario@mail.ro".toList
val email7 = "mario@mail.com".toList
val email8 = "mario@mail.org".toList
val email9 = "ben@mail.ro".toList
val email10 = "ben@notmail.z".toList


val emails : List[Email] = List(email1, email2, email3, email4 , email5)

/*
Slice out the part of the email from start till we find the '@' character
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

def getName(email: Email) : Email =
  email.slice(0, email.indexOf('@'))

/*
  Same as above, but for a list of emails
  */

def getNames (l: List[Email]): List[Email] =
  l.map((email: Email) => getName(email))

/*
  Same as getName, but here we start from the index of first the '.' character + 1, until the end of the email.
  so if we had ana@example.com, it would return only com
  */

def getTLD(email: Email) =
  email.slice(email.indexOf('.')+1,email.size)


/*
  Filter out emails that have the specified TLD, for each email in the list
  we check if their TLD matches the given TLD, if it doesn't we keep it , if it does we drop it
  */

def removeTLD(l: List[Email], tld: Str): List[Email] =

  l.filter((email: Email) => !getTLD(email).equals(tld))

/*
  uses groupBy function to group emails by their name, the function that "computes" the key here is the getName function
  */
def groupEmailsByName(l: List[Email]): List[List[Email]] =
  groupBy[Email,Email](l)( (email: Email) => getName(email) ).map{
    case (key, listOfEmails) => listOfEmails
  }


/*
  If there's no duplicates, forAll will return true but our function returns false, but when we actually have at least
  a single duplicate, forAll will return false, and we reverse it, meaning that it's true that we have duplicates
*/
def containsDuplicates(l: List[Email]): Boolean =
  !groupEmailsByName(l).forall((sameNameEmails: List[Email]) => sameNameEmails.size <= 1)


/*
  Group emails by their names, then use fold to go through the matrix, if any of the lists of emails that are grouped by the same name is bigger than 1, then we found a name that
  gets used multiple times in different emails, so we increase the accumulator
  */
def countDuplicates(l: List[Email]): Int =
  groupEmailsByName(l).foldRight[Int](0)(
     (sameNameEmails: List[Email], numOfDups: Int) => if(sameNameEmails.size > 1) numOfDups + 1 else numOfDups
    )

/*
  Group emails by name, then filter out the lists that do not have more than one email (meaning their names didn't get duplicated or used in multiple emails)
  */
def extractDuplicates(l: List[Email]): List[List[Email]] = {
  groupEmailsByName(l).filter( (sameNameEmails: List[Email]) => sameNameEmails.size > 1 )
}


// Converts emails back into strings
def res( emails: List[Email]) : List[String] =
  emails.map((email : Email) => email.mkString)

def resMap( matrix: List[List[Email]]) : List[List[String]] =
  matrix.map((listOfEmails: List[Email]) => res(listOfEmails) )

res(getNames(emails)) // should return ["ana", "ana", "jim", "mary", "mary"]

res(removeTLD(emails, "ro".toList)) // should return ana@aol.com, jim@cx.com, mary@mail.com

resMap(groupEmailsByName(List(email1,email3,email5,email8)))
resMap(groupEmailsByName(List(email1,email2)))
resMap(groupEmailsByName(List(email1, email4,email2, email5)))
resMap(groupEmailsByName(List(email1,email2, email3,email5, email3)))
resMap(groupEmailsByName(List(email1,email2,email4,email8, email9,email3, email7,email10)))


containsDuplicates(List(email1,email3,email5,email8)) // should return false
containsDuplicates(emails) // should return true
containsDuplicates(List(email1)) // should return false
containsDuplicates(List(email1, email2)) // should return true
containsDuplicates(List(email3,email4)) // should return false

countDuplicates(List(email1,email3,email5,email8)) // should return zero (no duplicates)
countDuplicates(List(email1,email2)) // should return 1
countDuplicates(List(email1, email4,email2, email5)) // should return 2
countDuplicates(List(email1,email2, email3,email5, email3)) // should return 2 too
countDuplicates(List(email1,email2,email4,email8, email9,email3, email7,email10)) // should return 3

resMap(extractDuplicates(List(email1,email2,email4,email8, email9,email3, email7,email10))) // ana, mario, ben only have duplicates



