package lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description: Unit = numbers match {
    case head :: Nil => println(s"the only element is $head.")
    case _ =>
  }

  class Person(val name:String, val age:Int, val height:Int)

  object Person {
    def unapply(p: Person): Option[(String, Int)] = if (p.age < 21) Some((p.name, p.age)) else None
    def unapply(age: Int): Option[String] = Some(if (age<21) "minor" else "major")
  }

  val bob = new Person("Bob", 18, 120)
  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old."
  }

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status." // unapply(age:Int) -> Option(String) is used
  }

  println(greeting)
  println(legalStatus)

  /*
  Exercise
  */

  val n = 4
  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "an even number"
    case _ => "no property"
  }

  object even {
    def unapply(n: Int): Option[Int] = if (n % 2 == 0) Some(n) else None
  }

  object singledigit {
    def unapply(n: Int): Option[Int] =  if (n < 10) Some(n) else None
  }

  val mathProp = n match {
    case singledigit(x) => s"single digit, $x"
    case even(x) => s"an even number, $x"
    case _ => "no property"
  }

  println(mathProp)

  //infix patterns
  case class Or[A, B](a:A, b:B) //Either
  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)

  //decomposing sequences using unapplySeq
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int]= Cons(1, Cons(2, Cons(3, Empty)))

  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1 and 2"
    case _ => "something else"
  }

  println(decomposed)

  //custom return types for unapply
  //The return type of unapply can be any type as long as it satisfies the property of having the two methods:
  // isEmpty: Boolean and get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false

      def get = person.name
    }
  }

    println(bob match {
      case PersonWrapper(n) => s"This person's name is $n"
      case _ => "An alien"
    })


}
