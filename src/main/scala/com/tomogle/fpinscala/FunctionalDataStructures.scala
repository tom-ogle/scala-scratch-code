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

}
