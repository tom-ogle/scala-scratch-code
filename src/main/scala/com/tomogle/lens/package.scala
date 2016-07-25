package com.tomogle

package object lens {
  trait LensI[A, B] {
    self =>

    val get: A => B
    def set: (A, B) => A

    def compose[C](l: Lens[B, C]): LensI[A, C] = new LensI[A, C] {
      override val get: A => C = a => (l.get compose self.get)(a)

      override def set: (A, C) => A = (a, c) => self.set(a, l.set(self.get(a), c))
    }

    def mod(f: B => B, a: A): A = self.set(a, f(self.get(a)))
  }
  case class Lens[A, B](get: A => B, set: (A, B) => A) extends LensI[A, B]

  def unitLens[A] = Lens[A, Unit](get = a => (), set = (a, _) => a)
  def self[A] = Lens[A, A](get = a => a, set = (a, _) => a)
  def member[K, V](k: K) = Lens[Map[K, V], Option[V]](
    get = m => m get k,
    set = {
      case (m, Some(v)) => m + (k -> v)
      case (m, None) => m
    }
  )
}
