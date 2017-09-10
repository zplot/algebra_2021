package algebra

import algebra.Utils.IntMap

import scala.io.{BufferedSource, Source}

object PruebaFiniteField  extends App {
  println("Empezamos")

  val cuerpo = FiniteField(7,3)


  implicit def convert1(x: Int): cuerpo.baseField.T2 = cuerpo.baseField.builder(x)


  val poly1 = cuerpo.polyRing.builder(Map(0 -> 3, 1 -> 2, 2 -> 3, 3 -> 4))

  val poly2 = anillo.builder(Map(1 -> 3, 2 -> 3))
}