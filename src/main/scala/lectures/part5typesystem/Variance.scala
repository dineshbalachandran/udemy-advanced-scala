package lectures.part5typesystem

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  //What is variance?
  //It is the problem of type substitution of generics,
  //i.e. if T inherits from S, what is the inheritance hierarchy of Generic[T] with respect to Generic[S]

  //if the inheritance is T from S:
  // 1. if Generic[T] from Generic[S] then it is CO-VARIANT
  // 2. if Generic[T] has no inheritance relationship at all with Generic[S] then it is INVARIANT
  // 3. if Generic[S] from Generic[T] then it is CONTRA-VARIANT

  //1. CO-VARIANT
  class CCage[+T]
  val ccage: CCage[Animal] = new CCage[Cat]

  //2. INVARIANT
  class ICage[T]
  //the below won't compile, icage will only accept an ICage[Animal] nothing else
  //val icage: ICage[Animal] = new ICage[Cat]

  //3. CONTRAVARIANT
  class XCage[-T]
  val xcage: XCage[Cat] = new XCage[Animal]


  //CLASS FIELDS Variance

  //'val class fields' are in the COVARIANT position, in this position
  //it only accepts invariant or covariant types
  //in the below first two cases T is either invariant or covariant
  //case 1.
  class InvariantCage[T](val animal: T)

  //case 2.
  class CovariantCage[+T](val animal: T)

  //case 3. does not work as type T is contravariant and can't be used as a field which has to be
  //covariant or invariant
  //In other words, Contravariant classes cannot have class fields!! Only Invariant and Covariant classes
  //can have val class fields.

  //class ContraVariantCage[-T](val animal: T)

  //This is to avoid situations like below
  //val catCage: ContraVariantCage[Cat] = new ContraVariantCage[Animal](new Crocodile)

  //The below var class field is in a CONTRAVARIANT position as well as a COVARIANT position!!
  //Therefore both the below are non compilable

  //1. class CovariantVariableCage[+T](var animal: T)
  // val ccage: CovariantVariableCage[Animal] = new CovariantVariableCage[Cat](new Cat)
  // ccage.animal = new Crocodile // this will stuff a Crocodile into a Cat cage.

  //2. class ContravariantVariableCage[-T](var animal: T)

  //Therefore the only acceptable type for a var class field is INVARIANT
  class InvariantVariableCage[T](var animal: T)

  //METHOD ARGUMENTS Variance

  //method arguments are in CONTRAVARIANT position
  trait AnotherCovariantCage[+T](val animal: T) {
    //the below will not compile, as type T which is covariant is used in a contravariant position
    // val aDogCage = new AnotherCovariantCage[Dog](new Dog)
    // val aCage: AnotherCovariantCage[Animal] = aDogCage
    // aCage.add(new Cat)
    // aDogCage.animal.wagTail() will fail as a Cat will have no wagTail method
    //def addAnimal(animal: T) = true
  }

  //whereas for Contravariant types the method addAnimal complies fine
  class AnotherContraVariantCage[-T] {
    def addAnimal(animal: T) = true
  }

  class Kitty extends Cat

  val accAnimal = AnotherContraVariantCage[Animal]
  val acc: AnotherContraVariantCage[Cat] = accAnimal
  acc.addAnimal(new Cat)
  acc.addAnimal(new Kitty)
  accAnimal.addAnimal(new Dog)
  //the below does not compile, even though accAnimal.addAnimal does which is equivalent of the below line
  //this is okay, rather causes no harm as there is no way in a contravariant type to access a field
  //to give access to new Dog.
  //acc.addAnimal(new Dog)

  //To be able to support method arguments on COVARIANT types, the below hack is used
  //i.e. accept a super type B of A indicated by >: and return a new generic of this super type
  //This is called widening the type

  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = new MyList[B] //widening the type
  }

  val emptyList = new MyList[Kitty]
  val animals = emptyList.add(new Kitty) //animals is a MyList[Kitty]
  val moreAnimals = animals.add(new Cat) //moreAnimals is a MyList[Cat]
  val evenMoreAnimals = moreAnimals.add(new Dog) //evenMoreAnimals is a MyList[Animal], not MyList[Dog]

  //RETURN TYPES Variance
  //Return types are in COVARIANT position
  class PetShop[-T] {
    //def get(isItaPuppy: Boolean): T //does not compile as T which is contravariant is in a covariant position

    //this avoids issues like the below
    //val catShop = new PetShop[Animal] {
    //                def get(isItaPuppy: Boolean): Animal = new Cat
    //                }
    //val dogShop: PetShop[Dog] = catShop
    //dogShop.get(true) // expect to get a dog will return a cat instead

    def get[S <: T](isItaPuppy: Boolean, defaultAnimal: S): S = defaultAnimal
  }

  val animalShop = new PetShop[Animal]
  val shop: PetShop[Dog] = animalShop
  //val evilCat1 = shop.get(true, new Cat) will not compile
  val evilCat1 = animalShop.get(true, new Cat)
  class TerraNova extends Dog
  val bigFurry1 = animalShop.get(true, new TerraNova) //this is fine
  val bigFurry2 = shop.get(true, new TerraNova) //this is also fine

  //Use Covariance = for a Collection of things
  //Use Contravariance = for a Group of actions

  /**
   * Invariant, Covariant and Contravariant Parking[T] (things List[T) as argument
   * 1. Parking[T](things: List[T]) {
   *  park(vehicle: T)
   *  impound(vehicles: List[T])
   *  checkVehicles(conditions: String): List[T]
   * }
   * Implement all 3 variants of this API to compare
   *
   * 2. use an InvariantList iList[T] instead
   * 3. Parking = monad!
   *    - flatMap addition
   */

  class IList[T]
  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle

  class InVariantParking[T](vehicles: List[T]) {
    def park(vehicle: T): InVariantParking[T] = ???
    def impound(vehicles: List[T]): InVariantParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[S](f: T => InVariantParking[S]): InVariantParking[S] = ???
  }

  class CoVariantParking[+T](vehicles: List[T]) { //covariant class field
    def park[S >: T](vehicle: S): InVariantParking[S] = ???
    def impound[S >: T](vehicles: List[S]): InVariantParking[S] = ???
    def checkVehicles(conditions: String): List[T] = ??? //covariant return
    def flatMap[S](f: T => CoVariantParking[S]): CoVariantParking[S] = ???
  }

  class ContraVariantParking[-T](vehicles: List[T]) {
    def park(vehicle: T) = ()
    def impound(vehicles: List[T]) = ()
    def checkVehicles[S <: T](conditions: String): List[S] = ???
    def flatMap[R <: T, S](f: R => ContraVariantParking[S]): ContraVariantParking[S] = ???
  }

  class CoVariantIListParking[+T](vehicles: IList[T]) {
    def park[S >: T](vehicle: S): CoVariantIListParking[S] = ???
    def impound[S >: T](vehicles: IList[S]): CoVariantIListParking[S] = ???
    def checkVehicles[S >: T](conditions: String): IList[S] = ??? //covariant return
  }

  class ContraVariantIListParking[-T](vehicles: IList[T]) {
    def park(vehicle: T): ContraVariantIListParking[T] = ???
    def impound[S <: T](vehicles: IList[S]): ContraVariantIListParking[S] = ???
    def checkVehicles[S <: T](conditions: String): IList[S] = ???
  }

}
