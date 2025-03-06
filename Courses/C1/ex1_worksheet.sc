import scala.annotation.tailrec

val x = 1 // immutable variable
val b = if(x>0) 1 else 0 // Always cover all conditions, don't leave a single if without an else
var y = 10 // we dont like that (its mutable)

// You can add or not add the curly braces
def f(x: Int): Int = x + 1

f(1)

def myAddition(x: Int, y: Int): Int = x + y
myAddition(1,2)

// Normal recursive function
def factorial(x : Int): Int = {

  if(x == 0) 1 else x * factorial(x-1)
}

/*a function that takes an interval [start.stop] and checks if there is a prime
 number on that interval */
def primeIn(start: Int, stop: Int): Boolean = {


  def isPrime(n: Int): Boolean = {
    /* we go from 2 until sqrt(n)
    */
    def loop (i: Int): Boolean = {
      if(i > n/2) true // the number is prime
      else if (n % i == 0) false // the number is divisible by  iterator
      else loop(i+1) // else we continue checking all numbers until n/2
    }
    loop(2)
  }

  if (start > stop) false // the interval has been traversed so no prime number has been found
  else if (isPrime(start)) true // the first number in the range is prime
  else primeIn(start+1, stop)

}

primeIn(14,18)


//the nth fibonacci number
/*
Its inefficient because a lot of numbers are getting recomputed again and again

suppose F(n) is number of recursive calls performed by fib,
F(n) = F(n-1) + F(n-2) + 1, because F(n) is also a call
Approximated: F(n) = 2* F(n-1) + 1
F(n) = O(2^n)
to avoid this we can use dynamic programming to store precomputed values
 */
def fib(n: Int): Int =
  if(n==0) 0
  else if (n == 1) 1
  else fib(n-1) + fib(n-2)

fib(10)


/*
Tail-end optimization of recursive functions,
the compiler will detect that theres no computation done inside
the funciton and the compiler replaces the funciton call instead of leaving them in the stack
 */

def betterFib(n: Int): Int = {
  def loop(i: Int,n_last: Int, n_bflast: Int): Int = {
    if (i >= n) n_bflast
    else loop(i+1,n_last + n_bflast, n_last)
  }
  loop(0,1,0)

}
betterFib(50)