package algebra

import algebra.Utils.IntMap

object PruebaPolynomialsOverFp extends App {

  println("Empezamos")

  val cuerpo = Fp(5)
  val anillo = PolynomialsOverFp(cuerpo)

  val poly1 = anillo.builder(IntMap(Map(1 -> 2)))




}