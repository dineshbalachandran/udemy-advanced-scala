package lectures.part2afp

object Monads extends App {


  //A monad is a type that defines a unit/pure/apply method as well as a flatMap method and satisfies the Monad laws
  //def apply(a: A): Monad[A]  essentially a constructor
  //def flatMap[B](f: A => B): Monad[B]
  //
  //The implementations of apply and flatMap should satisfy the below Monad laws:
  //1. Left identity: apply(a).flatMap(f) == f(a)
  //2. Right identity: m.flatMap(apply) == m        m is a monad instance
  //3. Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  //a Try like monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing]:
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this

  val attempt = Attempt {
    throw new RuntimeException("My own monad, yes!")
  }

  println(attempt)

  /**
   * Exercise
   * 1. Implement a Lazy[T] monad = computation which will only be executed when it's needed
   *
   * 2. Implement an alternative monad with a map + flatten in terms of flatMap
   * Monad[T] {
   *
   * def flatMap[B](f: T => Monad[B]): Monad[B] = ???
   * def map[B](f: T => B): Monad[B] = ???
   * def flatten(m: Monad[Monad[T]]): Monad[T] = ???
   */

  //1.
  class Lazy[A](t: => A) {
    lazy val compute = t
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(compute) // note the 'call by name' the input parameter of f
  }

  object Lazy {
    def apply[A](t: => A): Lazy[A] = new Lazy(t)
  }

  def computeValue() = {
    println("Hello")
    42
  }

  val l = Lazy(computeValue()) //the computeValue() is not executed as the Lazy.apply input parameter is 'call by name'
  val f = l.flatMap(x => Lazy(x))
  println(l.compute)
  println(f.compute)

  //2.
  class Monad[A](value: A) {
    def flatMap[B](f: A => Monad[B]): Monad[B] = ???
    def map[B](f: A => B): Monad[B] = flatMap(x => Monad(f(x)))
    def flatten(m: Monad[Monad[A]]): Monad[A] = m.flatMap(x => x)
  }

  object Monad {
    def apply[A](value: A): Monad[A] = new Monad(value)
  }


}
