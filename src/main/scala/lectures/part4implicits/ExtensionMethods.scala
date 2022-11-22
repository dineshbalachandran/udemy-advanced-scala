package lectures.part4implicits

import java.util.Date
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.annotation.targetName
import scala.util.{Failure, Success}

object ExtensionMethods extends App {

  case class Person(name: String) {
    def greet(): String = s"Hi, I'm $name, how can I help?"
  }

  extension (string: String) {
    def greetAsPerson(): String = Person(string).greet()
  }

  val greeting = "Dinesh".greetAsPerson()

  object Scala2Extensions {
    implicit class RichInt(val value: Int) {
      def isEven: Boolean = value % 2 == 0

      def isSqrt: Double = Math.sqrt(value)

      def times(f: () => Unit): Unit = (1 to value).foreach(_ => f())

      def *[T](l: List[T]): List[T] = (1 to value).flatMap(_ => l).toList
    }
  }

  val is3Even = 3.isEven

  //can take only one variable
  extension (value: Int) {

    def isEven: Boolean = value % 2 == 0

    def isSqrt: Double = Math.sqrt(value)

    def times(f: () => Unit): Unit = (1 to value).foreach(_ => f())

    def *[T](l: List[T]): List[T] = (1 to value).flatMap(_ => l).toList

  }

  //generic extensions
  extension [A](list: List[A]) {
    def ends: (A, A) = (list.head, list.last)
    def extremes(using Ordering[A]): (A, A) = list.sorted.ends
  }

  val l = List(1,2,5).ends
  println(l)
  //REWRITING JSONSerialization using extensions, the type class instances and enricher class are no longer
  //required, they are now replaced by extension methods in Scala 3
  //JSON domain
  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = s"\"$value\""
  }
  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {

    override def stringify: String = values.map {
      case (key, value) => s"\"$key\":${value.stringify}"
    }.mkString("{", ",", "}")

  }

  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  extension (value: String) {
    def toJson: JSONValue = JSONString(value)
  }

  extension (value: Int) {
    def toJson: JSONValue = JSONNumber(value)
  }

  extension (user: User) {
    def toJson: JSONValue = JSONObject(Map(
      "name" -> JSONString(user.name),
      "age" -> JSONNumber(user.age),
      "email" -> JSONString(user.email)
    ))
  }

  extension (post: Post) {
    def toJson: JSONValue = JSONObject(Map(
    "content" -> JSONString(post.content),
    "created" -> JSONString(post.createdAt.toString)
    ))
  }

  extension (feed: Feed) {
    def toJson: JSONValue = JSONObject(Map(
      "user" -> feed.user.toJson,
      "posts" -> JSONArray(feed.posts.map(_.toJson))
    ))
  }

  //test type classes and conversion
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@rockthejvm")
  val feed = Feed(john, List(
    Post("hello", now),
    Post("look at this cute puppy", now)
  ))

  println(feed.toJson.stringify)
  
  

  class P2PRequest
  class P2PResponse

  extension (request: P2PRequest) {
    def receive: Int = 42
    def future: Future[P2PRequest] = Future(request)
  }

  extension (response: P2PResponse) {
    def receive: Int = 24
    def future: Future[P2PResponse] = {
      val f = Future(response)
      f onComplete {
        case Success(r) => println(r.receive)
        case Failure(e) => println(e)
      }
      f
    }
  }


  val request = P2PRequest()
  println(request.receive)

  val response = P2PResponse()
  println(response.receive)


}
