package lectures.part5typesystem

object RockingInheritance extends App {

  trait Writer[T] {
    def write(value: T): Unit
  }

  trait Closeable {
    def close(status: Int): Unit
  }

  trait GenericStream[T] {
    def foreach(f: T => Unit): Unit
  }

  //Intersection argument type
  def processStream[T](stream: GenericStream[T] & Writer[T] & Closeable): Unit = {
    stream.foreach(println)
    stream.close(0)
  }

  class GenStream[T](value: T) extends GenericStream[T] with Writer[T] with Closeable {
    override def foreach(f: T => Unit): Unit = f(value)

    override def close(status: Int): Unit = ()

    override def write(value: T): Unit = ()
  }

  val s: GenStream[String] = GenStream("test")
  processStream(s)

  //diamond with multiple inheritance hierarchies
  trait Animal { def name: String }
  trait Lion extends Animal {override def name: String = "lion"}
  trait Tiger extends Animal {override def name: String = "tiger"}

  class Mutant extends Lion with Tiger

  val m = new Mutant
  println(m.name) //prints 'tiger'
  //Scala resolves the hierarchy for this method call to
  //Mutant
  //extends Animal with {override def name: String = "lion"}
  //with {override def name: String = "lion"}
  //i.e. the last override gets picked

  //the super problem + type linearization

  trait Cold {
    def print = println("cold")
  }

  trait Green extends Cold {
    override def print: Unit = {println("green"); super.print}
  }

  trait Blue extends Cold {
    override def print: Unit = {println("blue"); super.print}
  }

  class Red {
    def print = println("red")
  }

  class White extends Red with Green with Blue {
    override def print = {
      println("white")
      super.print
    }
  }

  val color = new White
  color.print //prints white, blue, green, cold
  //this is due to type linerization which resolves to
  //White = AnyRef with <body of Red>
  //        with (AnyRef with <body of Cold> with <body of Green>
  //        with (AnyRef with <body of Cold> with <body of Blue>
  //        with <body of White>
  //In the next pass this is reduced to
  //White = AnyRef
  //        with <body of Red>
  //        with <body of Cold>
  //        with <body of Green>
  //        with <body of Blue>
  //        with <body of White>
  // which results in white, blue, green, cold being printed (Red is not printed)


}
