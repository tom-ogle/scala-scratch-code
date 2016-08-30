package com.tomogle.magnetpattern

object MagnetPattern2 {

  val post: Route => Route = ???
  def originalHostMethod[T](obj: T)(implicit ev: T => String): Route => Route = ???

  // See http://spray.io/blog/2012-12-13-the-magnet-pattern/ for further details.
  // Can't combine post and originalHostMethod due to the implicit. This won't work:
  /* originalHostMethod("some.host") {
   *   post {
   *     ...
   *   }
   * }
   */
  // Would have to wrap the originalHostMethod call in parentheses which isn't elegant
  /* (originalHostMethod("some.host")) {
   *   post {
   *     ...
   *   }
   * }
   */
  // composableHostMethod gets round this with the magnet, you can write:
  /* composableHostMethod("some.host") {
   *   post {
   *     ...
   *   }
   * }
   */

  def composableHostMethod(magnet: HostMagnet) = magnet()

  trait Route

  sealed trait HostMagnet {
    def apply(): Route => Route
  }

  object HostMagnet {
    implicit def fromObj[T](obj: T)(implicit ev: T => String) =
      new HostMagnet {
        override def apply(): (Route) => Route = r => r
      }
  }

}
