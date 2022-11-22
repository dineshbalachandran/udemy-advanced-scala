package lectures.part3concurrency

object JVMConcurrencyProblems {

  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()

    //either 1 or 2 will print depending on which thread finishes last
    println(x)
  }

  case class BankAccount(var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.amount -= price
  }

  def buySync(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized {
      bankAccount.amount -= price
    }
  }

  //demonstrates how with mutable data the amount is left in a inconsistent state due to race condition
  //because bankAccount.amount -= price is not an atomic operation
  def demoBankingProblem(f: (BankAccount, String, Int) => Unit): Unit = {
    (1 to 10000).foreach { _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => f(account, "shoes", 3000))
      val thread2 = new Thread(() => f(account, "phone", 4000))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if (account.amount != 43000) println(s"AHA I've just broken the bank: ${account.amount}")
    }
    println("Demo banking problem finished")
  }


  /**
   * 1. Create 'inception threads'
   *  thread 1
   *    --> thread 2
   *      --> thread 3
   *        ...
   *  each thread prints "hello from thread $i", print in reverse order of thread creation
   */

  def inceptionThreads(n: Int): Unit = {
    def recursiveThreads(i: Int): Unit = {
      if (i > n) ()
      else {
        val thread = new Thread(() => recursiveThreads(i + 1))
        thread.start()
        thread.join()
        println(s"hello from thread $i")
      }
    }
    recursiveThreads(1)
  }

  //2. what's the max and min value of x below
  //max possible value that x may take is 100, while the min possible value that x may take is 1
  //max is all threads execute in a serial fashion, min if all threads read the value x = 0 at once and update
  //x to 1
  def minMaxX(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }

  //3. "sleep fallacy" what is the value of the message
  //almost always prints 'Scala is awesome'
  //However its possible that is not always the case, Thread.sleep yields the execution
  //it may so happen that the main thread and the awesomeThread both yield to the OS
  //and when the sleep time completes, it could be >1000 ms (say 2s), if main thread
  //gains control, the 'Scala sucks' message will print
  //the solution is to join the worker thread, which is aweSomeThread.join() before printing the message
  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "Scala is awesome"
    })

    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(1001)
    println(message)
  }


  def main(args: Array[String]): Unit = {
    //demoBankingProblem(buy) //can potentially leave the amount in an inconsistent state
    //demoBankingProblem(buySync) //state is consistent with an increased latency introduced

    inceptionThreads(50)
    //demoSleepFallacy()
  }

}
