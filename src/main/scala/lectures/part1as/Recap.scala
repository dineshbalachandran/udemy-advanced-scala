package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {
  val aCondition: Boolean = true
  val aConditionalVal: Int = if (aCondition) 42 else 65

  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  //Unit = void
  val theUnit = println("Hello scala")

  def aFunction(x: Int): Int = x + 1

  //recursion: stack and tail recursion
  @tailrec
  def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n-1, n * accumulator)

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch")
  }

  //method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog

  //anonymous classes
  val aCarnivore = new Carnivore:
    override def eat(a: Animal): Unit = println("roar")

  //generics
  abstract class MyList[+A] //covariant

  //singletons and companions
  object MyList //companion to class MyList

  //case classes
  case class Person(name: String, age: Int)

  //exceptions and try/catch/finally

  //val throwsException = throw new RuntimeException // type for this is Nothing
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception" + e.toString
  } finally {
    println("some logs")
  }

  //functional programming, functions internally are implemented as classes with apply method
  val incrementer = new Function1[Int, Int] {
    override def apply(v1:Int): Int = v1 + 1
  }

  incrementer(1)

  //this is a syntax sugared version of the above function
  val anonymousIncrementer = (x:Int) => x + 1

  List(1,2,3).map(anonymousIncrementer) //map is a higher order function

  //for comprehension is a syntactic sugar for flatMap, flatMap .... to map sequence
  //the below is essentially
  val pairs1 = List(1,2,3).filter(_%2==0).flatMap(num => List('a', 'b', 'c').map(char => num + "-" + char))
  println(pairs1)
  val pairs = for {
    num <- List(1,2,3) if num % 2 == 0
    char <- List('a','b','c')
  } yield num + "_" + char
  println(pairs)

  val aMap = Map(
    "Dinesh" -> 123,
    "Som" -> 768
  )

  //"collections": Options, Try
  val anOption = Some(2)

  //pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }


}
