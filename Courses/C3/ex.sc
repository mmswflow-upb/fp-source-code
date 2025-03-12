//We want to compute the sum of all values in a given interval
// Direct recursive function
def sumAll(start: Int, stop: Int): Int =
  if(start > stop) 0
  else start + sumAll(start+1, stop)


// Tail recursive function
def tailSumAll(start:Int, stop: Int) : Int = {
  def helperSumAll(i: Int, sum: Int): Int = {
    if(i > stop) sum
    else helperSumAll(i+1, sum+i)
  }

  helperSumAll(start, 0)
}

//tailSumAll(1,10)

// Sum of all values in a given interval but squared

// Tail recursive function
def tailSumSquared(start:Int, stop: Int) : Int = {
  def helperSumSquared(i: Int, sum: Int): Int = {
    if(i > stop) sum
    else helperSumSquared(i+1, sum+i)
  }

  helperSumSquared(start, 0)
}

// as you can see, its the same as the previous function but with a slight change in the parameters of the helper function
// (we're passing the square of the iterator)

//We can instead have a function that computes the sum of (fx0) + (fx1) + (fx2) + ... + (fxn) for a given function f and interval [a,b]

// Functions are also considered objects in Scala, so we can pass them as parameters to other functions
// f: Int => Int is a function that takes an Int and returns an Int

def sumWithF(f: Int => Int, start: Int, stop: Int): Int =
  def loop(i: Int, crtSum: Int): Int =
    if(i > stop) crtSum
    else loop(i+1, f(i) + crtSum)

  loop(start, 0)

// Identity function which returns the same value
def id(x: Int) : Int = x
//sumWithF(id, 0, 10)

// Square function
def square(x: Int): Int = x * x
//sumWithF(square, 0, 10)

//We can also use anonymous functions instead if we're not going to reuse the id or square functions

//Anonymous identity function and the way we read it is lambda x [pause], we can also not specify the type of x for very simple functions
sumWithF((x: Int) => x, 0, 10)

sumWithF((x) => x, 0, 10)
sumWithF(x => x, 0, 10)

//Higher order functions are the ones that take as input a function and return a function

// if we want to conceal or hide the algorithm that we're applying on the interval
// (for example in libraries, we want to spare the users the details from behind certain functions)
def alg1(x: Int): Int = x
def alg2(x: Int): Int = x * x
def alg3(x: Int): Int = x * x * x

def currySumWithF(alg: Int => Int): (Int, Int) => Int = {
  def sumWithF(start: Int, stop: Int) : Int = {
    def loop(i: Int, crtSum: Int): Int = {
      if(i > stop) crtSum
      else loop(i+1, alg(i) + crtSum)
    }
    loop(start, 0)
  }

  sumWithF
}

/* The purpose of this is to be able to build functions that apply given algorithms on any
range at any time, because maybe the range that we will apply that function on
wont be available immediately or we want to apply it on multiple ranges throughout
the program, so we dont have to call the function with all parameters again and again
we just supply a range to the function which already has the algorithm built into it
and we can pass it around throughout the program and files */

val applyAlg1 : (Int, Int) => Int  = currySumWithF(alg1) // or anonymous functions
val applyAlg2 = currySumWithF(alg2) // this is called functional closure,
val applyAlg3 = currySumWithF(alg3) // or anonymous functions

applyAlg1(1, 10)
// or
currySumWithF(alg1)(0,10)
//or
currySumWithF(x => x)(0,10)

//We can make the definition of the currySumWithF function look cleaner and shorter

// It takes parameters in turns (first f then range)
def cleanSumWithF(f: Int => Int)(start: Int, stop: Int): Int =
  def loop(i: Int, crtSum: Int): Int =
    if(i> stop) crtSum
    else loop(i+1, crtSum + f(i))

  loop(start, 0)


val applyAlg1Clean : (Int, Int) => Int  = cleanSumWithF(alg1) // or anonymous functions
val applyAlg2Clean = cleanSumWithF(alg2) // or anonymous functions
val applyAlg3Clean = cleanSumWithF(alg3) // or anonymous functions

def fcurry(x: Int)(y: Int)(z: Int): Int = x + y + z //curry functions in this style

/*
What's happening here, why is this type like this? Well it's because the fcurry function takes parameters in turn (curry-style), so
if you have val fCurryExamp = fcurry, without giving it any arguments, the type of this value would be int => (int => (int => int))
a function A that takes as input an int and returns a function B, function B takes as input an int and returns function C,
 function C takes as input an int and returns an int, thus we have four ints, and scala allows us to drop the parenthesis in this case,
because fcurry isn't a function that takes as input a number and returns another function that takes as input two numbers such as
def fCurry2(x : Int) : (Int, Int) => Int, we actually used curry-style to make this function take parameters in turns
* */
val fCurry0 : Int => Int => Int = fcurry(0)
val fCurry01: Int => Int = fcurry(0)(1) // same as fCurry0(1)

def fUncurry(x: Int, y: Int, z: Int): Int = x + y + z

// fUncurry(0,1,2) // no way to give a value later we must provide them at once

// function composition
// This is also curry syntax because it takes an int after providing the functions which will be composed
def compose(f: Int => Int, g: Int => Int): Int => Int = (x : Int) => f(g(x))

// another way instead of adding the lambda function as return type, we use currying
def compose2(f:Int=> Int, g: Int=> Int)(x: Int): Int = f(g(x))

//example to compute 2x + 1 for each number (xi) in a range then sum them all
val newAlg = cleanSumWithF(compose(x => x + 1, x => 2 * x))
newAlg(0, 10)
