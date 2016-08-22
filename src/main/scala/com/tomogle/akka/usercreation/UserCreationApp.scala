package com.tomogle.akka.usercreation

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import com.tomogle.akka.usercreation.RequestManager.CreateUser
import com.tomogle.akka.usercreation.UserChecker.{BadUser, CheckUser, GoodUser}
import com.tomogle.akka.usercreation.UserStorage.AddUser

import scala.language.postfixOps

case class User(name: String, emailAddress: String)

object RequestManager {
  sealed trait RequestManagerMessage
  case class CreateUser(user: User) extends RequestManagerMessage
}

class RequestManager(userChecker: ActorRef, userStorage: ActorRef) extends Actor {
  import scala.concurrent.duration._
  implicit val timeout = Timeout(5 seconds)
  import context.dispatcher

  import akka.pattern.ask
  override def receive: Receive = {
      case CreateUser(user) =>
        userChecker ? CheckUser(user) map {
      case GoodUser(goodUser) =>
        println(s"$goodUser was not in blacklist, attempting to add the user to storage")
        userStorage ! AddUser(goodUser)
      case BadUser(badUser) =>
        println(s"$badUser was in the blacklist, discarding request to add the user")
    }
  }
}

object UserChecker {
  sealed trait UserCheckerMessage
  case class CheckUser(user: User) extends UserCheckerMessage

  sealed trait UserCheckerResponse
  case class BadUser(user: User) extends UserCheckerResponse
  case class GoodUser(user: User) extends UserCheckerResponse
}

class UserChecker extends Actor {

  val emailBlacklist: Set[String] = Set("spammer@test.com", "spammer2@test.com")

  override def receive: Receive = {
    case CheckUser(user) => if (emailBlacklist.contains(user.emailAddress)) {
      println(s"$user was in the email blacklist")
      sender() ! BadUser(user)
    } else {
      println(s"$user was not in the email blacklist")
      sender() ! GoodUser(user)
    }
  }
}

object UserStorage {
  sealed trait AddUserMessage
  case class AddUser(user: User) extends AddUserMessage
}

class UserStorage extends Actor {

  private var users: List[User] = Nil
  override def receive: Receive = {
    case AddUser(user) =>
      users = user :: users
      println(s"Added $user to storage")
  }
}

object UserCreationApp extends App {
  val system = ActorSystem("UserCreation")
  val userChecker = system.actorOf(Props[UserChecker], "UserChecker")
  val userStorage = system.actorOf(Props[UserStorage], "UserStorage")

  val requestManager = system.actorOf(Props(new RequestManager(userChecker, userStorage)), "RequestManager")

  val wellBehavedUser = User("well behaved user", "me@test.com")
  requestManager ! CreateUser(wellBehavedUser)

  val blacklistedUser = User("naughty user", "spammer@test.com")
  requestManager ! CreateUser(blacklistedUser)
  TimeUnit.SECONDS.sleep(5)
  system.terminate()
}
