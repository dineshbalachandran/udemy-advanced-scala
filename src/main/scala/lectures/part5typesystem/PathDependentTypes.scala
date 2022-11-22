package lectures.part5typesystem

object PathDependentTypes extends App {

  class Outer {
    class Inner
    type InnerType
    object InnerObject

    def print(i: Inner) = println(i)
    def printGeneral(i: Outer#Inner) = println(i)
  }

  def aMethod: Int = {
    class HelperClass
    type HelperType = String //alias only, types can't be defined
    2
  }

  //the inner class types are per instance
  //here the inner types are outer1.Inner and outer2.Inner and they are of different types
  //therefore they are path-dependant
  val outer1 = new Outer
  val inner1: outer1.Inner = new outer1.Inner

  val outer2 = new Outer
  val inner2: outer2.Inner = new outer2.Inner

  //val inner3: outer2.Inner = new outer1.Inner //this is not allowed by the compiler
  outer1.print(inner1)
  //outer1.print(inner2) //not allowed
  outer2.print(inner2)

  outer1.printGeneral(inner2) //this is allowed, as Outer#Inner is a super type of all the Inner types

  /**
   * Exercise (Scala 2 only)
   * DB Keyed by Int or String, but maybe others
   *
   */

  trait ItemLike {
    type Key
  }

  trait Item[K] extends ItemLike {
    type Key = K
  }
  trait IntItem extends Item[Int]
  trait StringItem extends Item[String]

  //def get[ItemType <: ItemLike](key: ItemType#Key): ItemType = ??? //this is not accepted by the Scala 3 compiler

  //get[IntItem](42) //ok
  //get[StringItem]("home") //ok
  //get[IntItem]("scala") //not ok

}
