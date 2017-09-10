package algebra

import algebra.Utils.IntMap

object PruebaPolynomialsOverFp extends App {

  println("Empezamos")

  val cuerpo = Fp(7)
  val anillo = PolynomialsOverFp(cuerpo)

  implicit def convert1(x: Int): anillo.T0 = anillo.field.builder(x)

  val poly1 = anillo.builder(Map(0 -> 3, 1 -> 2, 2 -> 3, 3 -> 4))
  val poly2 = anillo.builder(Map(1 -> 3, 2 -> 3))

  println("poly1 = " + poly1)
  println("poly2 = " + poly2)

  val dif = poly1 - poly2
  println("dif = " + dif)



  val producto = poly1 * poly2

  println("producto = " + producto)

  val cociente = poly1 / poly2

  println("cociente = " + cociente)






}