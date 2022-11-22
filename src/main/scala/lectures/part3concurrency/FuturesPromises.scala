package lectures.part3concurrency

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration.*

//implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

object FuturesPromises {

//  def calculateMeaningofLife: Int = {
//    Thread.sleep(2000)
//    42
//  }
//
//  val aFuture = Future {
//    calculateMeaningofLife
//  }
//
//  println(aFuture.value) //should not usually be used
//
//  aFuture.onComplete { // a partial function
//    case Success(meaningoflife) => println(s"Meaning of life is $meaningoflife")
//    case Failure(e) => println(s"Failed with $e")
//  } //Some (i.e. any) thread runs this Future
//
//  //this is required to keep the main thread and thereby the application process alive to
//  //give the Future time to complete
//  Thread.sleep(3000)
//
//  //mini social network
//
//  case class Profile(id: String, name: String) {
//    def poke(anotherProfile: Profile) = println(s"${this.name} poking ${anotherProfile.name}")
//  }
//
//  object SocialNetwork {
//    //a map based database
//
//    val names = Map(
//      "fb.id.1-zuck" -> "Mark",
//      "fb.id.2-bill" -> "Bill",
//      "fb.id.0-dummy" -> "Dummy"
//    )
//
//    val friends = Map(
//      "fb.id.1-zuck" -> "fb.id.2-bill"
//    )
//
//    val random = new Random()
//
//    //API
//    def fetchProfile(id: String): Future[Profile] = Future {
//      Thread.sleep(random.nextInt(300))
//      Profile(id, names(id))
//    }
//
//    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
//      Thread.sleep(random.nextInt(400))
//
//      val bfId = friends(profile.id)
//      Profile(bfId, names(bfId))
//    }
//  }
//
//  //client: mark to poke bill
//  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
//  mark.onComplete {
//    case Success(markProfile) => {
//      val bill = SocialNetwork.fetchBestFriend(markProfile)
//      bill.onComplete {
//        case Success(billProfile) => markProfile.poke(billProfile)
//        case Failure(e) => e.printStackTrace()
//      }
//    }
//    case Failure(e) => e.printStackTrace()
//  }
//
//  Thread.sleep(1000)
//
//  //functional composition of futures
//  val nameOnTheWall = mark.map(profile => profile.name)
//
//  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
//
//  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))
//
//  //for comprehensions to chain Futures
//  for {
//    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
//    bill <- SocialNetwork.fetchBestFriend(mark)
//  } yield mark.poke(bill)
//
//  Thread.sleep(1000)
//
//  //fallbacks
//  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
//    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
//  }
//
//  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
//    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
//  }
//
//  val fallbackResult =
//    SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))
//
//  //blocking on Futures using Await.result - this blocks the calling thread unlike wait which yields and does not block
//  case class User(name: String)
//  case class Transaction(sender: String, receiver: String, amount: Double, status: String)
//
//  object BankingApp {
//    val name = "Rock the JVM Banking"
//
//    def fetchUser(name: String): Future[User] = Future {
//      Thread.sleep(500)
//      User(name)
//    }
//
//    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
//      Thread.sleep(1000)
//      Transaction(user.name, merchantName, amount, "SUCCESS")
//    }
//
//    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
//
//      val transactionStatusFuture = for {
//        user <- fetchUser(username)
//        transaction <- createTransaction(user, merchantName, cost)
//      } yield transaction.status
//
//      Await.result(transactionStatusFuture, 2.seconds) //blocks on the Future, not generally recommended to use
//    }
//  }
//
//  println(BankingApp.purchase("Dinesh", "iPhone", "store", 1000))
//
//


  /**
   * Exercises
   * 1. Fulfill a future immediately with a value
   * 2. inSequence(futurea, futureb)
   * 3. first(fa, fb) => new future with either fa or fb's result depending on which finishes first
   * 4. last(fa, fb) => new future with the last finishing value
   * 5. retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */

  //1
  def fulfillImmediately[T](value: T): Future[T] = Future(value) //or Future.successful(value)

  //2
  def inSequence[A, B](fa: Future[A], fb: Future[B]): Future[B] = fa.flatMap(_ => fb)

  //3
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]

    //since the promise can be fulfilled by either of the futures first and can be fulfilled only once
    //promise.tryComplete is called to ensure that no exceptions are raised
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future
  }

  //4
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    //1 promise both future's try to complete
    //2 promise will be completed by the last future
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    val checkAndComplete = (result: Try[A]) =>
    if(!bothPromise.tryComplete(result))
      lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

//  val fast = Future {
//    Thread.sleep(100)
//    42
//  }
//
//  val slow = Future {
//    Thread.sleep(200)
//    45
//  }
//
//  first(fast, slow).foreach(f => println(s"First $f"))
//  last(fast, slow).foreach(l => println(s"Last $l"))
//
//  Thread.sleep(1000)

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil(action, condition)
      }



  def main(args: Array[String]): Unit = {

    println("In main :" + Thread.currentThread().threadId())

    val random = new Random
    val action = () => Future {
      Thread.sleep(100)
      val nextValue = random.nextInt(100)
      println("In action: " + Thread.currentThread().threadId() + ", generated " + nextValue)
      nextValue
    }

    def condition(x:Int) = {
      println("In condition: " + Thread.currentThread().threadId())
      x < 50
    }
//    val f1 = action()
//    val f2 =
//      f1
//        .filter {
//          println("In filter: " + Thread.currentThread().threadId())
//          condition }
//        .foreach(r => {
//          println("In foreach: " + Thread.currentThread().threadId() + ", settled at " + r) })

    retryUntil(action, condition).foreach(r => {
      println("In foreach: " + Thread.currentThread().threadId() + ", settled at " + r)
    })

    Thread.sleep(1000)

//    //Promise future pattern
//    val promise = Promise[Int]
//    val future = promise.future
//
//    //consumer
//    future.onComplete {
//      case Success(r) => println(s"[consumer] I've received $r")
//      case Failure(e) => println(s"[consumer] something went wrong .. $e")
//    }
//
//    //producer
//    val producer = new Thread(() => {
//      println("[producer] crunching numbers ...")
//      Thread.sleep(500)
//      promise.success(42) //or promise.failure
//      println("[producer] done")
//    })
//
//    producer.start()
//
//    Thread.sleep(1000)

  }


}
