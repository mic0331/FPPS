def fact(n: Int): Int = if (n == 0) 1 else n * fact(n - 1)


// re-write the sum function

def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)

  sumF
}

def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)
def sumFactorials = sum(fact)

sumCubes(1, 10) + sumFactorials(10, 20)

// avoiding the middle man
def cube(x: Int): Int = x * x * x
sum(cube)(1, 10)

// therefore we can re-write the sum function

def sum_(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum_(f)(a + 1, b)

// therefore
// def f(args1) ... (args n-1) = { def g(args n) = E; g}
// or ...
// def f(args1) ....(args n-1) = (arg sn => E)


def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

product(x => x * x)(3, 4)

def factorial(n: Int) = product(x => x)(1, n)
fact(5)

def mapReduct(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduct(f, combine, zero)(a + 1, b))

def product_(f: Int => Int)(a: Int, b: Int): Int = mapReduct(f, (x, y) => x * y, 1)(a, b)

product_(x => x * x)(3, 8)