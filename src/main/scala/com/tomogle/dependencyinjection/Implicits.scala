package com.tomogle.dependencyinjection

object Implicits {

  case class User(id: Long, username: String)

  trait UserRepository {
    def get(id: Long): User

    def find(username: String): User
  }

  trait UserRetrieval {
    def getUser(id: Long)(implicit userRepository: UserRepository) = userRepository.get(id)
    def findUser(username: String)(implicit userRepository: UserRepository): User = userRepository.find(username)
  }



}
