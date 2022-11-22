package lectures.part4implicits

object PimpMyLibrary extends App {

  //the any val extension is for compiler optimisation of Val types (Int, Char etc.)
  //note String is not an AnyVal
  implicit class RichInt(val value: Int) extends AnyVal {
    def isEven: Boolean = value % 2 == 0

    def isSqrt: Double = Math.sqrt(value)

    def times(f: () => Unit): Unit = (1 to value).foreach(_ => f())

    def *[T](l: List[T]): List[T] = (1 to value).flatMap(_ => l).toList
  }

  42.isEven //type enrichment = pimping

  /*
  - Enrich String class
  - asInt
  - encrypt (John to Lnjp)

  Keep enriching Int class
  - times(function)
  - 3.times(() => ...)
  - * (multiply method that takes a list as argument

  - 3 * List(1,2) => List(1,2,1,2,1,2)
  */

  implicit class RichString(value: String) {
    def asInt: Int = value.toInt

    def encrypt: String = value.map(a => (a + 2).toChar)
  }

  println("42".asInt)
  println("John".encrypt)

  3.times(() => println("hi there"))
  println(3 * List(1, 2))
}