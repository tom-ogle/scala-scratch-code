package com.tomogle.fpinscala

import scala.annotation.tailrec

object FunctionalDataStructures {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case h :: t => f(h, foldRight(t, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  // foldLeft in terms of foldRight
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((b, a) => f(a, b))

  // foldRight in terms of foldLeft
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a, b) => f(b, a))

  def reverse[A](as: List[A]): List[A] = {
    @tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case h :: t => loop(t, h :: acc)
    }
    loop(as, Nil)
  }

  // Reverse in terms of foldleft
  def reverse2[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => a :: b)

  // Reverse in terms of foldRight
  def reverse3[A](as: List[A]): List[A] = {

    def loop(a: A, b: List[A]): List[A] = {
      b match {
        case Nil => a :: Nil
        case h :: t => h :: loop(a, t)
      }
    }
    foldRight(as, Nil: List[A])(loop)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => 1 + b)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => 1 + b)

  def sum[A](as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product[A](as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case h :: Nil => Nil
    case h :: t => h :: init(t)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case _ :: t => t
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => a :: Nil
    case _ :: t => a :: t
  }

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case _ :: t => drop(t, n - 1)
    }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case h :: t => if (f(h)) dropWhile(t)(f) else h :: t
  }

  // append in terms of foldLeft
  def append[A](xs: List[A], ys: List[A]): List[A] = foldLeft(reverse(xs), ys)((b, a) => a :: b)

  // append in terms of foldLeft
  def append2[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)((a, b) => a :: b)

  def concat[A](xs: List[List[A]]): List[A] = foldRight[List[A], List[A]](xs, Nil)((a, b) => append2(a, b))

  // concat implemented directly with more efficient runtime
  def concat2[A](xs: List[List[A]]): List[A] = {
    println("Looping")
    xs match {
      case Nil => Nil
      case h1 :: t1 => h1 match {
        case Nil => concat2(t1)
        case h2 :: t2 => h2 :: concat2(t2 :: t1)
      }
    }
  }

  def plusOne(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case h :: t => h + 1 :: plusOne(t)
  }

  def doubleToString(ls: List[Double]): List[String] = ls match {
    case Nil => Nil
    case h :: t => h.toString :: doubleToString(t)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case h :: t => f(h) :: map(t)(f)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case h :: t => if (f(h)) h :: filter(t)(f) else filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case h :: t => append(f(h), flatMap(t)(f))
  }

  // filter in terms of flatMap
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap[A, A](as)(a => {
    if (f(a)) List(a) else Nil
  })

  def sumCorrespondingElements(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => h1 + h2 :: sumCorrespondingElements(t1, t2)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def loop(ls: List[A], subls: List[A]): Boolean = (ls, subls) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (h1 :: t1, h2 :: t2) if h1 == h2 => loop(t1, t2)
      case _ => false
    }
    if (sup == Nil && sub == Nil) true
    else
      sup match {
        case Nil => false
        case h :: t =>
          val thisResult = loop(sup, sub)
          if (thisResult) true
          else hasSubsequence(t, sub)
      }
  }

  sealed trait Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def sizeOfTree[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + sizeOfTree(left) + sizeOfTree(right)
  }

  def maxTreeValue(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maxTreeValue(left) max maxTreeValue(right)
  }

  def maxTreeDepth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => (maxTreeDepth(left) max maxTreeDepth(right)) + 1
  }

  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) =>
      Branch(mapTree(left)(f), mapTree(right)(f))
  }

  def foldTree[A, B](tree: Tree[A], z: A => B)(f: (Tree[A], Tree[A]) => B): B = tree match {
    case Leaf(value) => z(value)
    case Branch(left, right) => f(left, right)
  }

  def sizeOfTree2[A](tree: Tree[A]): Int = foldTree(tree, {_: A => 1}) {
    (left, right) => 1 + sizeOfTree2(left) + sizeOfTree2(right)
  }

  def maxTreeValue2(tree: Tree[Int]): Int = foldTree(tree, (value: Int) => value) {
    (left, right) => maxTreeValue2(left) max maxTreeValue2(right)
  }

  def maxTreeDepth2[A](tree: Tree[A]): Int =
    foldTree(tree, {_: A => 1}) {
      (left, right) => (maxTreeDepth2(left) max maxTreeDepth2(right)) + 1
    }

  def mapTree2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    val zFunc: A => Tree[B] = value => Leaf(f(value))

    foldTree(tree, zFunc) {
      (left, right) => Branch[B](mapTree2(left)(f), mapTree2(right)(f))
    }
  }

}
