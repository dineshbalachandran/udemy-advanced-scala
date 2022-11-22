package lectures.part4implicits


object TypeClasses extends App {

  //The Ordering trait is an example of a 'type class'. A type class is a trait that takes a type and
  //describes what operations can be applied to that type.

  trait HTMLWritable { //note this takes no type - therefore this is not a type class
    def toHTML: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHTML: String = s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  val html = john.toHTML

  //with the above definition of trait, the following limitations are present:
  //1 - can only be applied to those classes to which we have source code access
  //2 - it can only support a single implementation, i.e. toHTML cannot return different results depending on
  //conditions

  //another option is to write an object that handles this for all data types
  object HTMLSerializerPM {
    def toHTML(value: Any): String = value match {
      case User(n, a, e) => "User"
      case List(1,x) => "Tail"
      case _ => ""
    }
  }
    //the drawback is that type safety is lost and limitation #2 still persists
    //though limitation #1 above is resolved

  //HTMLSerializer is a type,
  //the UserSerializer and DateSerializer are type instances (they are defined as objects)
  //all limitations are eliminated
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  object UserSerializer extends HTMLSerializer[User] {
    override def serialize(user: User): String =
      s"<div>${user.name} (${user.age} yo) <a href=${user.email}/> </div>"
  }

  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString}</div"
  }

  println(UserSerializer.serialize(john))


  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer:HTMLSerializer[T]): String = serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }

  println(HTMLSerializer.serialize(42))

  //uses the HTMLSerializer apply method, to use the IntSerializer's serialize method
  println(HTMLSerializer[Int].serialize(42))

  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println(john.toHTML) //println(new HTMLEnrichment[User](john).toHTML(UserSerializer)
  println(2.toHTML)

  //A general TYPE class example/pattern
  //1. The type class definition
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  //2. Instances of the type class
  implicit object MyTypeClassTemplateInt extends MyTypeClassTemplate[Int] {
    def action(value: Int): String = value.toString
  }

  //3. Implicit class to convert a class to the type class
  implicit class MyTypeClassTemplateEnricher[T](value: T) {
    def action(implicit mytypeclass: MyTypeClassTemplate[T]): String = mytypeclass.action(value)
  }

  //the Int is converted to the enriched type, on which the Int type instance action is called
  println(25.action)

  //to be able to invoke the Int type instance in the manner below
  //a companion object is required to 'surface' the implicit type instance
  //this is an alternate option of usage to the above pimping style of usage.
  //
  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }

  println(MyTypeClassTemplate[Int].action(25))
  println(125.action)


  /**
   * Equality
   */

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.apply(a, b)
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  //can't make this implicit - it will need to be put in a different package
  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }

  val anotherJohn = User("John", 66, "email")
  println(Equal(john, anotherJohn)) //AD-HOC polymorphism

  /*
  Exercise
  - Improve Equal TC with the implicit conversion class with two methods
   ===(another value: T)
  !==(another value: T)

  */

  implicit class EqualEnricher[T](value: T) {
    def ===(anotherValue: T)(implicit equal: Equal[T]): Boolean = equal(value, anotherValue)
    def !==(anotherValue: T)(implicit equal: Equal[T]): Boolean = ! ===(anotherValue)
  }

  /*
  john.===(anotherJohn)
  new EqualEnricher[User](john).===(anotherJohn)
  new EqualEnricher[User](john).===(anotherJohn)(NameEquality)
   */
  println(john === anotherJohn)
  println(john !== anotherJohn)

  // context bounds
  def htmlHeader[T](content: T)(implicit serializer: HTMLSerializer[T]): String =
    s"<html><body> ${content.toHTML(serializer)}</body></html>"


  //[T: HTMLSerializer]
  // require 'T' to be a HTMLSerializer - i.e. context bind it
  def htmlSugar[T: HTMLSerializer](content: T): String = {
    val serializer = implicitly[HTMLSerializer[T]]
    s"<html><body> ${content.toHTML}</body></html>"
    // or alternatively if required the below option is available, though not usually required
    //s"<html><body> ${content.toHTML(serializer)}</body></html>"
  }

  //implicitly
  case class Permissions(mask: String)
  implicit val defaultPermissions: Permissions = Permissions("0744")

  //in another part of the code, to surface which implicit is used, use implicitly
  val standardPerms = implicitly[Permissions]






}
