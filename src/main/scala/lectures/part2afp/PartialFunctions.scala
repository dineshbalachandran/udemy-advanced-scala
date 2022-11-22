package lectures.part2afp

object PartialFunctions extends App {

  //A partial function, is a function that only operates on a subset of the domain of its input
  //It is a unary function i.e. it can only accept one input parameter.

  val aTotalFunction = (x:Int) => x+1 //note that this is an example of single abstract method syntax sugar on Function1
                                      //trait; which is its apply method

  //For the below function only 1, 2 and 3 are valid input (i.e. only a subset of the Int domain)
  val aTotalFunctionActuallyPartial = (x:Int) => x match {
    case 1 => 42
    case 2 => 56
    case 3 => 99
  }

  //the below is a syntactic sugar specific to Partial functions, the right side is partial function value
  //Note that unlike Function which has Function1, Function2 variants, there is only PartialFunction
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 3 => 99
  }

  //Partial function methods
  println(aPartialFunction.isDefinedAt(67)) //false
  println(aPartialFunction.isDefinedAt(2)) //true

  //lift to a total function
  val lifted = aPartialFunction.lift //Int => Option[Int]

  //chaining, a partial function value is input to the orElse function
  val pfchain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfchain(2))
  println(pfchain(45))
  //println(pfchain(99)) this will still fail with a Match exception

  //a partial function extends Function therefore it can be used wherever a function is expected

  val aMappedList = List(1,2,3) map  {
    case 1 => 42
    case 2 => 78
    case 3 => 99
  }

  /**
   * 1 - Construct a partial function instance with an anonymous class instantiation
   * 2 - dumb chatbot as a partial function
   */

  val aChatbot = new PartialFunction[String, String] {
    
    val aSet = Set(1,2,3)

    val answers = Map (
      "hi" -> "Hi, how may I help you?",
      "name" -> "My name is chatbot"
    )

    override def apply(v1: String): String = answers(v1)

    override def isDefinedAt(x: String) = answers.contains(x)

  }

  scala.io.Source.stdin.getLines().foreach(line => println(aChatbot(line)))





}
