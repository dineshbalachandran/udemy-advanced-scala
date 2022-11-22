package lectures.part5typesystem

object TypeMembers extends App {

  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    type AnimalType //abstract type member
    type BoundedAnimal <: Animal //upper bounded in Animal
    type SuperBoundedAnimal >: Dog <: Animal //lower bounded in Dog and upper bounded in Animal
    type AnimalC = Cat //type alias
  }

  val ac = new AnimalCollection
  //val dog: ac.AnimalType = ???

  //val cat: ac.BoundedAnimal = new Cat //does not compile
  val pup: ac.SuperBoundedAnimal = new Dog //only allows Dog

  val cat: ac.AnimalC = new Cat

  //an alternate way to represent a generic using an abstract type member T
  trait MyList {
    type T
    def add(element: T): MyList
  }

  class NonEmptyList(value: Int) extends MyList {
    override type T = Int //notice the use of override for the type T
    def add(element: Int): MyList = ???
  }

  type CatsType = cat.type //derive the type of the cat

  /**
   * Exercise, enforce a type to be applicable to some types only
   * Particularly make this trait applicable to Numbers only and not don't allow it for others like String etc
   */
  trait MList {
    type A
    def head: A
    def tail: MList
  }


  //Exercise objective is to make the below not compile
//  class CustomList(hd: String, tl: CustomList) extends MList {
//    override type A = String
//    def head: String = hd
//    def tail = tl
//  }


  trait ApplicableToNumbers {
    type A <: Number
  }

  //the below does not compile
//  class CustomList(hd: String, tl: CustomList) extends MList with ApplicableToNumbers {
//    override type A = String
//    def head: String = hd
//    def tail = tl
//  }

  //Exercise objective is to make the below still compile
  class IntList(hd: java.lang.Integer, tl: IntList) extends MList with ApplicableToNumbers {
    override type A = java.lang.Integer
    def head = hd
    def tail = tl

  }

}
