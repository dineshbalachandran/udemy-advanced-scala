package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {

  /*
   The producer consumer problem
   The producer produces data 'x' while the consumer consumes the data/acts on the data
   producer  -> [ x ] -> consumer
   */

  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0

    def set(newValue: Int) = value = newValue

    def get = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons() = {
    val container = new SimpleContainer
    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      while (container.isEmpty) //scala indent used instead of braces
        println("[consumer] actively waiting ...")
      println("[consumer] I have consumed " + container.get)
    })
    val producer = new Thread(() => {
      println("[producer] computing ...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work, the value " + value)
      container.set(value)
    })

    consumer.start()
    producer.start()
  }
  //naiveProdCons()

  def smartProdCons() = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting ...")
      container.synchronized {
        //suspends execution of the thread and releases the lock on container, till some other thread notifies
        //resumption. Here container is the shared resource
        container.wait() //wait should only be called with a synchronized block
      }

      //
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work ...")
      Thread.sleep(500)
      val value = 42
      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify() //should only be called within a synchronized block,
      }
    })

    consumer.start()
    producer.start()
  }

  //smartProdCons()

  /*
  producer -> [x x x] -> consumer
  */

  def prodConsLargeBuffer() = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()

      while(true) {
        buffer.synchronized { //the method is part of
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting ...")
            buffer.wait()
          }

          //there must be at-least ONE value in the buffer
          val x = buffer.dequeue()
          println("[consumer] consumed " + x)

          //notify the producer thread (essentially any thread waiting on buffer) to produce something
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread (() => {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting ...")
            buffer.wait()
          }

          //there must be at least ONE EMPTY SPACE in the buffer
          println("[producer] producing " + i)
          buffer.enqueue((i))

          //notify the consumer thread (essentially any thread waiting on buffer) to consume something
          buffer.notify()
          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }
  //prodConsLargeBuffer()

  /*
  Prod-cons level 3 (multiple producers and multiple consumers)
  producer1 -> [? ? ?] -> consumer1
  producer2 -> [? ? ?] -> consumer2
  */

  def prodConsLevel3() = {
    def createConsumer(name: String, buffer: mutable.Queue[Int]) = new Thread(() => {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[consumer $name] buffer empty, waiting ...")
            buffer.wait()
          }

          //there must be at-least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer $name] consumed $x" )

          //notify the producer threads (essentially all threads waiting on buffer)
          buffer.notifyAll()
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    def createProducer(name: String, buffer: mutable.Queue[Int], startValue: Int) = new Thread (() => {
      val random = new Random()
      var i = startValue

      while(true) {
        buffer.synchronized {
          while (buffer.size == 3) {
            println(s"[producer $name] buffer is full, waiting ...")
            buffer.wait()
          }

          //there must be at least ONE EMPTY SPACE in the buffer
          println(s"[producer $name] producing $i")
          buffer.enqueue(i)

          //notify the consumer threads (essentially any thread waiting on buffer)
          buffer.notifyAll()
          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val consumers = List(createConsumer("A", buffer), createConsumer("B", buffer))
    val producers = List(createProducer("1", buffer, 0), createProducer("2", buffer, 100))

    consumers.foreach(_.start())
    producers.foreach(_.start())
  }

  //prodConsLevel3()

  /*
  Exercises:
  1. When is notify() different from notifyall()
  2. create a deadlock
  3. create a livelock
  */

  //notifyALL
  //creates 10 consuming threads and 1 producing thread,
  //if notify is used instead of notifyall, then 9 consuming threads will keep waiting for ever
  //notifyall will not lead to that situation
  def testNotifyAll() = {

    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[Thread $i] waiting ...")
        bell.wait()
        println(s"[Thread $i] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[Announcer] Rock n Roll")
      bell.synchronized {
        bell.notifyAll()
      }
    }).start()
  }

  //testNotifyAll()

  //2. Deadlock
  case class Friend(name: String) {
    def bow(other: Friend) = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }

    def rise(other: Friend) = {
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }
    }

    var side = "right"
    def switchSide() = {
      if (side == "right") side = "left"
      else side = "right"
    }

    def pass(other: Friend) = {
      while(this.side == other.side) {
        println(s"$this: Oh, but please, $other, feel free to pass")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

  //new Thread(() => sam.bow(pierre)).start() // sam's lock | then tries to get pierre's lock, which it is not able to get
  //new Thread(() => pierre.bow(sam)).start() //reverse case, resulting in a deadlock


  //3.Livelock
  //both thread yield to each other, without doing any work
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()


}
