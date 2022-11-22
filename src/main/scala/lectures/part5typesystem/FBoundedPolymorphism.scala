package lectures.part5typesystem




object FBoundedPolymorphism extends App {

  object Problem {

    trait Animal {
      def breed: List[Animal]
    }

    class Cat extends Animal {
      def breed: List[Animal] = ???
    }

    class Dog extends Animal {
      def breed: List[Cat] = ??? // the compiler does not complain to such usage
    }

  }

  object Solution1 {
    //this solution is often used in ORM
    //Entity[E <: Entity[E]]

    trait Animal[A <: Animal[A]] { // recursive type; F-Bounded Polymorphism
      def breed: List[A]
    }

    class Cat extends Animal[Cat] {
      def breed: List[Cat] = ???
    }

    class Dog extends Animal[Dog] {
      //def breed: List[Cat] = ??? // the compiler complains to this usage
      def breed: List[Dog] = ???
    }

    class Crocodile extends Animal[Dog] {
      override def breed: List[Dog] = ??? //the compiler does not complain
    }

  }

  //add a self type constraint to get over the Crocodile extending any arbitrary Animal type
  object Solution2 {

    // recursive type; F-Bounded Polymorphism
    trait Animal[A <: Animal[A]] { self : A => //self type
      def breed: List[A]
    }

    class Cat extends Animal[Cat] {
      def breed: List[Cat] = ???
    }

    class Dog extends Animal[Dog] {
      //def breed: List[Cat] = ??? // the compiler complains to this usage
      def breed: List[Dog] = ???
    }

    class Crocodile extends Animal[Crocodile] {
      override def breed: List[Crocodile] = ??? //the compiler complains if anything other than Crocodile is used
    }

    trait Fish extends Animal[Fish]
    class Shark extends Fish {
      override def breed : List[Fish] = List(new Cod) //this compiles, though it should not be allowed
    }

    class Cod extends Fish {
      override def breed: List[Fish] = ???
    }

  }

  //Type class based solution for the above issue in Solution2
  object Solution3 {
    //1. type class
    trait Animal[A] { //pure type classes
      def breed(animal: A): List[A]
    }

    class Dog //note Dog does not extend Animal[A], type classes are not inherited
    object Dog {
      //type class instance
      given DogAnimal: Animal[Dog] with {
        override def breed(dog: Dog): List[Dog] = List()
      }
    }

    class Cat
    object Cat {
      given CatAnimal: Animal[Cat] with {
        override def breed(cat: Cat): List[Cat] = List()
      }
    }

    //type class enricher
    extension [A: Animal](animal: A) {
      def breed(using a: Animal[A]): List[A] = a.breed(animal)
    }


  }

  import lectures.part5typesystem.FBoundedPolymorphism.Solution3.Dog
  import lectures.part5typesystem.FBoundedPolymorphism.Solution3.Cat

  val dog: Dog = new Dog
  println(dog.breed)

  val cat: Cat = new Cat
  println(cat.breed)




}
