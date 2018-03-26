// fixed point : f(x) = x
// for some functions f we can locate the fixed points by starting with
// an initial estimate and then by applying f in a repetitive way.
// x, f(x), f(f(x)), f(f(f(c)))), ...
// until the value does not vary anymore

import math.abs

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    //println("guess = " + guess)
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)

// going back to the Newton's function for sqrt, we can derive from this fixedPoint
// method that sqrt(x) is a fixedPoint of the function (y => x / y)

def sqrt(x: Double) =
  fixedPoint(y => x / y)(1)

// we have an infinite loop because the value oscillate between 1 and 2
// sqrt(2)

// to fix this problem we are stabilizing by averaging
def sqrt_(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)
sqrt_(2)

// this works so let's abstract it
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt__(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1)

sqrt__(2)