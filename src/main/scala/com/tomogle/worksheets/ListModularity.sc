val ints = List(1,2,3,4,5,6,7,8,9,10)

ints.foldRight(0)(_ + _)

val bools = List(false, false, false, true)

val anyTrue = bools.foldRight(false)(_ | _)

ints.foldRight[List[Int]](Nil)(_ :: _)

val countR: (Int, Int) => Int = (a, b) => b + 1
val countL: (Int, Int) => Int = (a, b) => a + 1
ints.foldRight(0)(countR)
ints.foldLeft(0)(countL)

val double: Int => Int = 2 * _

def andCons[T]: (T => T) => T => List[T] => List[T] =
  f => t => ts => f(t) :: ts

val doubleAndCons: (Int, List[Int]) => List[Int] = (x, xs) => andCons(double)(x)(xs)


ints.foldRight[List[Int]](Nil)(doubleAndCons)

