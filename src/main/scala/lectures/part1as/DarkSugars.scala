package lectures.part1as

import scala.util.Try

object DarkSugars {

  //1: method with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgMethod {
    //write some complex code in this code block expression
    42
  }

  val aTryInstance = Try {
    throw new RuntimeException
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  //2: single abstract method, instances of traits with single method can be reduced to lambdas
  trait Action {
    def act(x: Int): Int
  }

  val aInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x:Int) => x + 1 //same as above using single abstract method sugar

  //example Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello Scala")
  })

  val aSweeterThread: Thread = new Thread(() => println("sweet, Scala"))

  //a class with only one method not implemented
  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  //same sugar can be used for this class as well
  val anAbstractInstance: AnAbstractType = (a: Int) => println(s"$a sweet")

  //3, the :: and #:: methods which are special

  val prependedList = 2 :: List(3, 4) //equivalent to List(3, 4).::(2)
  //the last character of the method if it is :, then that method is right associative

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val mystream: MyStream[Int] = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  //#4, multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")

  lilly `and then said` "Scala is so sweet"

  //#5: infix types (used primarily in Type level programming
  class Composite[A, B]
  val composite: Composite[Int, String] = ???
  val anotherComposite: Int Composite String = ??? //equivalent to above

  class -->[A, B]
  val towards: Int --> String = ??? //same as -->[Int, String]

  //#6 update() is special similar to apply()
  val anArray = Array(1,2,3)
  anArray(2) =  7 //same as anArray.update(2, 7)

  //#7 setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0
    def member: Int = internalMember //getter
    def member_=(value: Int): Unit =
      internalMember = value //setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 //calls the aMutableContainer.member_=(42)

}
