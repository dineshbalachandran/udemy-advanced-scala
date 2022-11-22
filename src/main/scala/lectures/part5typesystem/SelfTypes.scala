package lectures.part5typesystem

import java.awt.image.ComponentColorModel

object SelfTypes extends App {

  //Self types are a way of requiring a type to be mixed in

  trait Instrumentalist {
    def play(): Unit
  }

  //with this 'marker' we state that any class implementing Singer will also need to implement Instrumentalist
  trait Singer { self : Instrumentalist =>
    def sing(): Unit
  }

  //the compiler will not allow LeadSinger to be created without mixing in Instrumentalist
  class LeadSinger extends Singer with Instrumentalist {
    override def sing(): Unit = ???
    override def play(): Unit = ???
  }

  //another example
  val jamesHetfield = new Singer with Instrumentalist {
    override def sing(): Unit = ???
    override def play(): Unit = ???
  }

  //The self type method can be used to manage dependencies between unrelated types where extends does not make sense
  //This method of 'dependency injection' is called a CAKE PATTERN

  //classical DI
  class Component
  class ComponentA extends Component
  class ComponentB extends Component

  class DependentComponent(val component: Component)

  //CAKE
  trait ScalaComponent {
    def action(x: Int): String
  }

  trait ScalaDependentComponent { self: ScalaComponent =>
    def dependentAction(x: Int): String =  action(x) + " this rocks!"
  }

  trait ScalaApplication { self: ScalaDependentComponent with ScalaComponent => }

  //Example 2
  trait Picture extends ScalaComponent
  trait Stats extends ScalaComponent

  //layer2
  trait Profile extends ScalaDependentComponent with Picture
  trait Analytics extends ScalaDependentComponent with Stats

  //layer 3
  trait AnalyticsApp extends ScalaApplication with Analytics

  //seemingly cyclic dependencies are okay, since they are not exactly
  // cyclic when they are implemented

  trait X { self: Y => }
  trait Y { self: X => }

}
