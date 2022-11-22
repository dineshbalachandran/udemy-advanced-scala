package lectures.part2afp

object CurriesPAF extends App {

  //curried functions, a function that returns a function - this can be used to implement a closure
  val superAddr: Int => Int => Int = x => y => x + y

  val add3 = superAddr(3) // return y => 3 + y
  println(add3(5))
  println(superAddr(3)(5))

  def curriedAddr(x:Int)(y:Int): Int = x + y

  //an ETA expansion or lifting is performed by the compiler to convert the method to a function
  val add4 = curriedAddr(4)

  //Exercise
  val simpleAddFunction = (x:Int, y:Int) => x + y
  def simpleAddMethod(x:Int, y:Int): Int = x + y
  def curriedAddMethod(x:Int)(y:Int) = x + y

  //add7: Int => Int = y => 7 + y

  val add7_1 = simpleAddFunction(7, _)
  val add7_2 = simpleAddMethod(7, _)
  val add7_3 = curriedAddMethod(7)
  val add7_4 = simpleAddMethod.curried(7)
  val add7_5 = simpleAddFunction.curried(7)
  val add7_6 = (x:Int) => simpleAddFunction(7, x)

  //Partial application of function allows to take a more general function and specialise it
  //like a closure. It is only necessary to leave one argument in the partial function application
  def concatenator(a: String, b: String, c: String): String = a + b + c
  val insertname = concatenator("Hello, I'm ", _ , ", how are you?")
  println(insertname("Dinesh"))

  val fillInTheBlanks = concatenator("Hello, ", _, _)

  /**
   * 1. Process a list of numbers and return their string rep
   * Use %4.2f, %8.6f, %14.12f formats with a curried formatter function
   */

  val l = List(1.0, 2.0, 3.4)
  def formatString(f: String)(x: Double) = f.format(x)
  def format4_2 = formatString("%4.2f")
  def format8_6 = formatString("%8.6f")
  def format14_12 = formatString("%14.12f")

  l.foreach(x => println(format4_2(x)))
  l.foreach(x => println(format8_6(x)))
  l.foreach(x => println(format14_12(x)))

  /**
   * Difference between 1. functions vs methods and 2. parameters: by-name and 0-lambda
   *
   */

  //The by name parameter is evaluated each time it is used, and not evaluated if not used
  //It is useful when expression like a code-block expression is required to be passed
  //
  def byName(n: => Int) = n+1
  def byFunction(f: () => Int) = f() + 1 //0-lambda

  def method: Int = 42
  def parenMethod(): Int = 42

  /**
   * calling byName and byFunction
   * - int
   * - method
   * - parenMethod
   * - lambda
   * - PAF
   */

  println(byName(2))
  println(byName(method))
  println(byName(parenMethod()))
  //println(byName(() => 1)
  println(byName((() => 1)())) //ok, the lambda is called, the result of which is passed to byName
  //println(byName(parenMethod _))

  //println(byFunction(2))
  //println(byFunction(method)) //no-ETA expansion here, not ok to pass-in
  println(byFunction(parenMethod)) //ETA expansion done, ok to pass-in
  println(byFunction(() => 1))
  println(byFunction(parenMethod _)) //forced ETA expansion, ok





}
