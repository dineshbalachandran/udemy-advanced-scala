package lectures.part4implicits

object ImplicitsIntro extends App {

  //The string 'Dinesh' is implicitly converted to an ArrowAssoc instance which has a method '->'
  //which returns a Tuple
  //This implicit class is present in the scala.Predef package
  val pair = "Dinesh" -> "48"
  val intPair = 1 -> 2

  case class Person(name: String) {
    def greet = s"Hi, my name is $name!"
  }

  implicit def fromStringToPerson(str: String): Person = Person(str)

  //the compiler tries to find match among the implicits for the method greet,
  //which it finds in the implicit method that takes a String and returns a Person
  //if it finds multiple matches, the compiler will error
  println("Peter".greet)

  //implicit parameters
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount: Int = 10

  increment(2) //the implicit parameter takes the value 10
}
