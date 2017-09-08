package algebra

import algebra.Utils.IntMap

object PruebaPolynomialsOverFp extends App {

  println("Empezamos")

  val cuerpo = Fp(5)
  val anillo = PolynomialsOverFp(cuerpo)

  val poly1 = anillo.builder(Map(1 -> 2, 2 -> 3, 3 -> 4))
  val poly2 = anillo.builder(Map(1 -> 3, 2 -> 3))

  println(poly1)
  println(poly2)

  val cociente = poly1 / poly2

  println(cociente)






}