package com.tomogle.dependencyinjection

import scalaz.Reader

object ReaderMonad {

  case class User(id: Long, username: String)

  trait UserRepository {
    def get(id: Long): User

    def find(username: String): User
  }

  // Reader[UserRepository, User] is a UserRepository => User function with
  // some sugar (e.g. map and flatMap to allow use in for comprehensions)

  trait UserRetrieval {
    def getUser(id: Long): Reader[UserRepository, User] = Reader { (userRepository: UserRepository) =>
      userRepository.get(id)
    }

    def findUser(username: String): Reader[UserRepository, User] = Reader { (userRepository: UserRepository) =>
      userRepository.find(username)
    }
  }
}
