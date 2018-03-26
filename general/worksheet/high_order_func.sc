def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a + 1, b)

sumInts(25, 602)


def cube(x: Int): Int = x * x * x

def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

sumCubes(25, 569)

def fact(n: Int): Int =
  if (n == 0) 1 else n * fact(n - 1)

def sumFactorials(a: Int, b: Int): Int =
  if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

sumFactorials(25, 169)

// summing with higher-order functions

// let's define ...
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

// where ...
def id(x: Int): Int = x

// we can then write ...
def sumInts_(a: Int, b: Int) = sum(id, a, b)
def sumCubes_(a: Int, b: Int) = sum(cube, a, b)
def sumFactorials_(a: Int, b: Int) = sum(fact, a, b)

// using anonymous functions
def sumInts__(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes__(a: Int, b: Int) = sum(x => x * x * x, a, b)

// the sum function uses linear recursion, let's re-write it using tail-recursion
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }

  loop(a, 0)
}