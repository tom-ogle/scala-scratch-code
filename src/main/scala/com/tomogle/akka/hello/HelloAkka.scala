package com.tomogle.akka.hello

import akka.actor.{Actor, ActorSystem, Props}

case class WhoToGreet(name: String)
class Greeter extends Actor {
  override def receive = {
    case WhoToGreet(name) => println(s"Hello $name")
  }
}
object HelloAkka extends App {

  val system = ActorSystem("Hello-Akka")
  val greeter = system.actorOf(Props[Greeter], "My-Greeter")

  greeter ! WhoToGreet("Tom")
}
