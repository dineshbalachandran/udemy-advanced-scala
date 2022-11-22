package lectures.part4implicits

object OrganisingImplicits extends App {

  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  //uses implicit ordering in scala.Predef package, if no implicit defined locally like the one above
  println(List(1, 5, 2, 3, 6).sorted)

  /*
  Implicits used in implicit parameters can be either of
  - val or var
  - object
  - accessor methods (like the one above) - i.e. defs that have no parentheses
  */

  // Exercise
  case class Person(name: String, age: Int)

  val persons = List(Person("Steve", 30), Person("Amy", 22), Person("John", 66))

  implicit def personOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)

  println(persons.sorted)

  /*
  Implicit scope priority high to low
  - LOCAL scope
  - imported scope
  - companions of all types involved in the method signature
    for e.g.     def sorted[B >: A](implicit ord: Ordering[B]) : List[B]
    The compiler will look for the implicit in
    - List
    - Ordering
    - in A or any superType

  */

  /*
  Exercise
  - totalPrice = usually used (50%) - put this in the companion object, since this is most common use case
  - by unit count (25%) - put this in its own object, which can be imported specifically
  - by unit price (25%) - put this in its own object
  */

  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    implicit def order: Ordering[Purchase] =
      Ordering.fromLessThan((a, b) => (a.nUnits * a.unitPrice) < (b.nUnits * b.unitPrice))
  }

  object CountOrdering {
    implicit def order: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }

  object PriceOrdering {
    implicit def order: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }

}
