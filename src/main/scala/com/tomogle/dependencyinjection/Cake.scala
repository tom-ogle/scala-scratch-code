package com.tomogle.dependencyinjection

object Cake {
  case class User(id: Long, username: String)

  trait UserRepositoryComponent {

    def userRepository: UserRepository

    trait UserRepository {
      def get(id: Long): User

      def find(username: String): User
    }
  }

  trait HardCodedUserRepositoryComponent extends UserRepositoryComponent {
    override def userRepository = new HardCodedUserRepository

    class HardCodedUserRepository extends UserRepository {
      override def get(id: Long): User = User(id, "hard-coded-user")

      override def find(username: String): User = User(1234L, username)
    }
  }

  trait UserRetrieval {
    this: UserRepositoryComponent =>

    def getUser(id: Long): User = userRepository.get(id)

    def findUser(username: String): User = userRepository.find(username)
  }

  object UserRetrievalImpl extends UserRetrieval with HardCodedUserRepositoryComponent

}
