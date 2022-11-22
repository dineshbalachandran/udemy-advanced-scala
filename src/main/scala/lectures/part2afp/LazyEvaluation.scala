package lectures.part2afp

import lectures.part2afp.LazyEvaluation.{Cons, EmptyStream, MyStream}

import scala.annotation.tailrec
import scala.util.Try

object LazyEvaluation extends App {

  lazy val x = {
    println("Hello")
    42
  }

  //Implications:
  //1.
  //Hello will print once to console, not twice
  //the lazy evaluation happens once on first use, subsequent uses use the saved value
  println(x)
  println(x)


  //Call by need, can be implemented using a combination of call by name and lazy val
  //2.
  //Call by name evaluates the parameter each time the parameter is used, in this case 'n'
  //Call by name is most useful when a function is passed in which needs to be evaluated each time it is called.
  def byNameMethod(n: => Int) = n + n + n + 1
  def computeValue() = {
    println("computing ...")
    42
  }

  //prints 'computing ...' thrice
  println(byNameMethod(computeValue()))

  def callByNeed(n: => Int) = {
    lazy val t = n
    t + t + t + 1
  }

  //prints 'computing ...' once
  println(callByNeed(computeValue()))

  //For comprehensions use withFilter which is a lazy val version of the filter method when using if guards
  val res = for {
    a <- List(1,2,3) if (a % 2 == 0)
  } yield a + 1

  println(res)

  //the above is equivalent to
  val res1 = List(1,2,3).withFilter(_ % 2 == 0) //does not evaluate the filter but returns a 'lazy object'
  println(res1) // only prints the object not the result
  res1.map(_ + 1).foreach(println) //prints the result

  /**
   * Exercise: implement a lazily evaluated, singly linked STREAM of elements
   *
   * naturals = MyStream.from(1)(x => x + 1) =  stream of natural numbers (potentially infinite!)
   * naturals.take(100).foreach(println) //lazily evaluated stream of the first 100 naturals (finite stream)
   * naturals.foreach(println) // will crash - infinite
   * naturals.map(_*2) //stream of all even numbers (potentially infinite)
   */

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] //prepend operator
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] //concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] //takes the first n elements out of this stream
    def takeAsList(n: Int): List[A] = take(n).toList()

    @tailrec
    final def toList[B >: A](acc: List[B] = List.empty): List[B] =
      if (isEmpty) acc.reverse
      else tail.toList(head :: acc)
  }

  object EmptyStream extends MyStream[Nothing] {

    def isEmpty = true

    override def head: Nothing = throw java.util.NoSuchElementException()

    override def tail: MyStream[Nothing] = throw java.util.NoSuchElementException()

    override def #::[B >: Nothing](element: B): MyStream[B] = Cons(element, this)

    override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

    override def foreach(f: Nothing => Unit): Unit = ()

    override def map[B](f: Nothing => B): MyStream[B] = this

    override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

    override def take(n: Int): MyStream[Nothing] = this
  }

  class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {

    override def isEmpty: Boolean = false

    override val head: A = hd

    override lazy val tail: MyStream[A]  = tl

    override def #::[B >: A](element: B): MyStream[B] = Cons(element, this)

    override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = Cons(head, tail ++ anotherStream)

    override def foreach(f: A => Unit): Unit = {
      @tailrec
      def foreachTailrec(stream: MyStream[A]): Unit = {
        if (stream.isEmpty) ()
        else {
          f(stream.head)
          foreachTailrec(stream.tail)
        }
      }

      foreachTailrec(this)
    }

    override def map[B](f: A => B): MyStream[B] = Cons(f(head), tail map f)

    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

    override def filter(predicate: A => Boolean): MyStream[A] = {
      if (predicate(head))
        Cons(head, tail filter predicate)
      else
        (tail filter predicate)
    }

    override def take(n: Int): MyStream[A] = {
      if (n <= 0)
        EmptyStream
      else if (n == 1)
        Cons(head, EmptyStream)
      else
        Cons(head, tail.take(n-1))}
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = Cons(start, MyStream.from(generator(start))(generator))
  }
}

object StreamsPlayGround extends App {

  val a = Cons(1, EmptyStream)
  val b = Cons(2, a)


  val naturals = MyStream.from(1)(_ + 1)
  b.take(1).foreach(println)
  b.take(2).foreach(println)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val from0 = 0 #:: naturals
  println(from0.head)

  from0.take(1000000).foreach(println)

  from0.map(_*2).take(100).toList().foreach(println)
  from0.flatMap(x => new Cons(x, new Cons(x+1, EmptyStream))).take(10).toList().foreach(println)
  from0.filter(_<10).take(10).toList().foreach(println)

  //Exercises
  //1 - stream of Fibonacci numbers
  //2 - stream of prime numbers using Eratosthenes' sieve

  /**
   *
   *
   */

  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] = new Cons(first, fibonacci(second, first+second))

  fibonacci(1,1).take(100).toList().foreach(println)

  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new Cons(numbers.head, eratosthenes(numbers.tail.filter(_%numbers.head != 0)))

  eratosthenes(MyStream.from(2)(_+1)).take(100).toList().foreach(println)

}
