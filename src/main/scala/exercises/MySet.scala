package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  def apply(v1: A): Boolean = contains(v1)

  //Implement a functional set
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /**
   * 1. remove an element
   * 2. intersection with another set
   * 3. difference with another set
   */

  def -(elem: A): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]

  def unary_! : MySet[A] //negation of a set or complement of this set

}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = PropertyBasedSet[A](_ => true)

}

// mathematical definition: all elements of type A that satisfies a property
// x is in A such that property(x) is satisfied (i.e. true)
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  //math definition: x in A | property(x) + elem
  override def +(elem: A): MySet[A] = PropertyBasedSet[A](x => property(x) || x == elem)

  override def ++(anotherSet: MySet[A]): MySet[A] = PropertyBasedSet[A](x => property(x) || anotherSet(x))

  //map, flatmap, foreach can't be implemented for a property based set
  override def map[B](f: A => B): MySet[B] = throw new IllegalArgumentException("Really deep rabbit hole")
  override def flatMap[B](f: A => MySet[B]): MySet[B] = throw new IllegalArgumentException("Really deep rabbit hole")
  override def foreach(f: A => Unit): Unit = throw new IllegalArgumentException("Really deep rabbit hole")

  override def filter(predicate: A => Boolean): MySet[A] = PropertyBasedSet[A](x => property(x) && predicate(x))

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def unary_! : MySet[A] = PropertyBasedSet[A](x => !property(x))
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = head == elem || (tail contains elem)

  override def +(elem: A): MySet[A] = if (!contains(elem)) NonEmptySet(elem, this) else this

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(head)) (tail filter predicate) + head else tail filter predicate

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = if (head == elem) tail else tail - elem + head

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
//The above filter(anotherSet) is equivalent to the below, conceptually intersection is the same as filtering
//on anotherSet
//    if (anotherSet.contains(head))
//      NonEmptySet(head, tail & anotherSet)
//    else
//      tail & anotherSet

  override def --(anotherSet: MySet[A]): MySet[A] = tail -- (anotherSet - head)

  override def unary_! : MySet[A] = PropertyBasedSet[A](x => !contains(x))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, EmptySet[A])
  }

}

object MySetPlayGround extends App {

  val l = List(1,3,4)
  Set(2,1,0).map{l}.foreach(println)
  println(l.apply(1))


  val s = MySet(1,2,3,4)
  s ++ MySet(-1, -2) + 5 + 3 flatMap (x => MySet(x, x*10)) filter (_%2==0) foreach println
  println(s(1))

  val negative = !s
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_%2==0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))

  val e = MySet[Int]()
  val n = e -- s

  println(n(1))
  println(s(1))
}
