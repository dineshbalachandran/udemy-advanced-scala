package lectures.part4implicits

object Givens extends App {

  //1. Implicit val
  val aList = List(2,4,3,1)

  //Scala 2
  object Implicits {
    implicit val descOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  }
  val anOrderedList = aList.sorted

  //Scala 3 replacement
  //the below will be applied to sorted above, even though it is defined later, since it is in the same scope
  given descOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)

  object GivensV2 {
    given descOrdering_v2: Ordering[Int] = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = y - x
    }
  }

  //using 'with' as below is equivalent to the above GivensV2
  object GivensV3 {
    given descOrdering_v3: Ordering[Int] with {
      override def compare(x: Int, y: Int): Int = y - x
    }
  }

  import GivensV3.given //imports all given GivenV3._ does not import all givens

  //2. Implicit arguments
  def extremes[A](list: List[A])(implicit ordering: Ordering[A]): (A, A) = {
    val sortedlist = list.sorted
    (sortedlist.head, sortedlist.last)
  }

  //Scala 3 equivalent to the above with 'using'
  def extremesV2[A](list: List[A])(using ordering: Ordering[A]): (A, A) = {
    val sortedList = list.sorted
    (sortedList.head, sortedList.last)
  }

  //3. Implicit def is usually used to synthesize new implicit values from other implicit vals
  trait Combinator[A] {
    def combine(a: A, b: A): A
  }

  //two implicits simpleOrdering and combinator are used to create new implicit Ordering[List[A]]
  implicit def listOrdering[A](implicit simpleOrdering: Ordering[A], combinator: Combinator[A]): Ordering[List[A]] =
  new Ordering[List[A]] {
    override def compare(x: List[A], y: List[A]) = {
      val sumX = x.reduce(combinator.combine)
      val sumY = y.reduce(combinator.combine)
      simpleOrdering.compare(sumX, sumY)
    }
  }

  //Scala 3 equivalent with given
  given listOrderingV2[A](using simpleOrdering: Ordering[A], combinator: Combinator[A]): Ordering[List[A]] with {
    override def compare(x: List[A], y: List[A]) = {
      val sumX = x.reduce(combinator.combine)
      val sumY = y.reduce(combinator.combine)
      simpleOrdering.compare(sumX, sumY)
    }
  }

  //implicit conversions with implicit def - Scala 2
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name."
  }

  implicit def string2Person(s: String): Person = Person(s)
  val greeting = "Dinesh".greet()

  //Scala 3 equivalent
  import scala.language.implicitConversions
  given string2PersonConversion: Conversion[String, Person] with {
    override def apply(s: String) = Person(s)
  }

  //Takeaway:
  //1. given and given/with is equivalent to implicit vals and implicit defs
  //2. using is equivalent to implicit arguments
  //3. Extension methods are equivalent to implicit classes

}
