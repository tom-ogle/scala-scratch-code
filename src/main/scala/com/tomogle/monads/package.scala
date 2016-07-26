package com.tomogle

package object monads {

  trait MyOption[+A] {
    def get(): A
    def isEmpty: Boolean

    def flatMap[B](f: A => MyOption[B]): MyOption[B] =
      if(isEmpty) MyNone
      else f(get())
  }
  object MyOption {
    def apply[T](t: T): MyOption[T] =
      if (t == null) MyNone
      else MySome(t)
  }
  case object MyNone extends MyOption[Nothing] {
    override def get(): Nothing = throw new NoSuchElementException("Cannot get a MyNone")

    override def isEmpty: Boolean = true

  }
  case class MySome[+T](t: T) extends MyOption[T] {
    override def get(): T = t

    override def isEmpty: Boolean = false

  }

}
