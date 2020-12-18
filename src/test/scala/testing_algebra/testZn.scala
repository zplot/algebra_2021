package testing_algebra
import algebra2.Ring
import algebra2.Zn

object testZn extends App {
  println("Empezamos")

  val ring = new Zn(10)


  println(ring.zeroDivisors)
  println(ring.units2)

}