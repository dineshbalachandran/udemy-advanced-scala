package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {

  //JVM threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel ...")
  })

  aThread.start() //this starts a JVM thread that runs on an OS thread

  //
  aThread.join() //blocks the current thread till aThread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  //the 'hello' and 'goodbye' messages are interleaved in a random order, and not sequential
  //due to the variability in scheduling of threads
  threadHello.start()
  threadGoodbye.start()

  //starting a thread is an expensive process
  //the solution is to use a pre-defined pool of threads

  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("something in the thread pool"))

  pool.execute (() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 seconds")
  })
  
  //use shutdown to close the thread pool for any further executions
  //this allows the running thread to complete, only prevents new executions
  pool.shutdown()
  //the below line will throw an exception
  //pool.execute(() => println("after pool shutdown"))


}
