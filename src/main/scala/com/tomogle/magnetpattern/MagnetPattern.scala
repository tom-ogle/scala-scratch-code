package com.tomogle.magnetpattern

/**
  * The original implementation, without the magnet pattern.
  */
trait PreMagnetPattern {
  def preMagnetFunction(s: String): String = s.concat("xyz")
  def preMagnetFunction(i: Int): Long = (i + 1).toLong
  def preMagnetFunction(l: Long): Long = l + 100
}

/**
  * Replacement implementation with magnet pattern.
  */
trait MagnetPattern {
  def magnetFunction(magnet: SampleMagnet): magnet.Result = magnet()
}

object SampleMagnet {
  implicit def fromString(s: String): SampleMagnet =
    new SampleMagnet {
      override type Result = String
      override def apply(): String = s.concat("xyz")
    }

  implicit def fromInt(i: Int): SampleMagnet =
    new SampleMagnet {
      override type Result = Long
      override def apply(): Long = (i + 1).toLong
    }

  implicit def fromLong(l: Long): SampleMagnet =
    new SampleMagnet {
      override type Result = Long
      override def apply(): Long = l + 100
    }
}

sealed trait SampleMagnet {
  type Result
  private[magnetpattern] def apply(): Result
}
