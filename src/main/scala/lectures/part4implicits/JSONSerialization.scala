package lectures.part4implicits

import java.util.Date

object JSONSerialization extends App {

  /* Social network context,
    Entities: Users, Posts, Feeds, introduce the ability to serialize the objects to JSON
    Solution steps:
    json domain classes, Business domain classes, Converters to map/transform business domain to JSON
    i.e. map Business entities to json (JSONValue) using type class converters

    //The advantage of this pattern is that it does not pollute the business entities or the json domain class

   */

  /*
  1 - Need to create JSON data types: Int, String, List, Date, user defined types
  2 - type classes for conversion to intermediate data types
  3 - serialize to JSON
  */

  //Business domain
  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

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

  //test JSON object
  val data = JSONObject(Map(
    "user" -> JSONString("Dinesh"),
    "posts" -> JSONArray(List(
      JSONString("Scala rocks"),
      JSONNumber(453)
    ))
  ))
  println(data.stringify)



  //2. Converter type classes
  /*
  1 - type class
  2 - type class instances
  3 - pimp library method to use type classes
  */

  //2.1 type class
  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }

  //2.3 enricher class
  implicit class JSONOps[T](value: T) {
    def toJSON(implicit converter: JSONConverter[T]): JSONValue = converter.convert(value)
  }

  //2.2 type class instances
  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object IntConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }

  implicit object UserConverter extends JSONConverter[User] {
    override def convert(user: User): JSONValue = JSONObject(Map(
      "name" -> JSONString(user.name),
      "age" -> JSONNumber(user.age),
      "email" -> JSONString(user.email)
    ))
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(post: Post): JSONValue = JSONObject(Map(
      "content" -> JSONString(post.content),
      "created" -> JSONString(post.createdAt.toString)
    ))
  }

  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(feed: Feed): JSONValue = JSONObject(Map(
      "user" -> feed.user.toJSON, 
      "posts" -> JSONArray(feed.posts.map(_.toJSON)) 
    ))
  }

  //test type classes and conversion
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@rockthejvm")
  val feed = Feed(john, List(
    Post("hello", now),
    Post("look at this cute puppy", now)
  ))

  println(feed.toJSON.stringify)

  def sendPost[A: JSONConverter](payload: A): JSONValue = payload.toJSON

}


