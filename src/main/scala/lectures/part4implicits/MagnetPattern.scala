package lectures.part4implicits

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object MagnetPattern extends App {

  // Magnet pattern solves problems caused by method overloading.
  // This involves the use of implicits and type classes

  class P2PRequest
  class P2PResponse
  class Serializer[T]


  //Issues created:
  //1. type erasure restricts overloading
  //2. lifting does not work
  //  val receiveFV = receive _ // does not work
  //3. code duplication within the implementation of receive
  //4. type inference and default args can't be used
  trait Actor {
    def receive(status: Int): Int
    def receive(request: P2PRequest): Int
    def receive(request: P2PResponse): Int
    def receive[T: Serializer](message: T): Int //context bound
    def receive[T: Serializer](message: T, statusCode: Int): Int
    def receive(future: Future[P2PRequest]): Int
    //def receive(future: Future[P2PResponse]): Int //won't compile due to type erasure
  }

  trait MessageMagnet[Result] {
    def apply(): Result
  }

  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
    def apply(): Int = {
      println("Handling P2P request")
      42
    }
  }

  implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
    def apply(): Int = {
      println("Handling P2P response")
      24
    }
  }

  receive(new P2PRequest) //converted to MessageMagnet[P2PRequest] implicitly
  receive(new P2PResponse)

  //Benefits of Magnet pattern

  //1 - no more type erasure issues
  implicit class FromResponseFuture(future: Future[P2PResponse]) extends MessageMagnet[Int] {
    def apply(): Int = {
      future.onComplete {
        case Success(r) => println(receive(r))
        case Failure(e) => println(e)
      }
      2
    }
  }

  implicit class FromRequestFuture(future: Future[P2PRequest]) extends MessageMagnet[Int] {
    def apply(): Int = 3
  }

  val request = Future(new P2PRequest)
  println(receive(request))
  println(receive(Future(new P2PResponse)))
//with the limitation that the 'magnet' trait can't take a type

  trait MathLib {
    def add1(x: Int) = x + 1
    def add1(x: String) = x.toInt + 1
    //other overloads
  }

  //magnetize MathLib
  trait AddMagnet {
    def apply(): Int
  }

  def add1(magnet: AddMagnet): Int = magnet()

  implicit class AddInt(x: Int) extends AddMagnet {
    override def apply(): Int = x + 1
  }

  implicit class AddString(x: String) extends AddMagnet {
    override def apply(): Int = x.toInt + 1
  }

  val addFv = add1 _ //this works if no type is used, if a type were present this will not work
  println(addFv(1))
  println(addFv("3"))

  //Drawbacks
  //1 - verbose
  //2 - harder to read
  //3 - no name arguments or default arguments
  //4 - call by name does not work correctly

  class Handler {
    def handle(s: => String) = {
      println(s)
      println(s)
    }
  }

  trait HandleMagnet {
    def apply(): Unit
  }

  def handle(magnet: HandleMagnet) = magnet()

  implicit class StringHandler(s: => String) extends HandleMagnet {
    def apply() = {
      println(s)
      println(s)
    }
  }

  def sideEffectMethod(): String = {
    println("Hello scala")
    "magnet"
  }

  handle(sideEffectMethod())
  handle({
    println("Hello scala")
    "magnet"
  })

}
