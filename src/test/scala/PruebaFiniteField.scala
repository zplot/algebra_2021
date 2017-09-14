package algebra

import scala.language.implicitConversions

import algebra.Utils.IntMap



object PruebaFiniteField  extends App {
  println("Empezamos")

  val cuerpo = FiniteField(7,3)


  implicit def convert1(x: Int): cuerpo.baseField.T2 = cuerpo.baseField.builder(x)
  implicit def convert2(x: Map[Int,cuerpo.baseField.T2]): cuerpo.polyRing.T1 = x.asInstanceOf[cuerpo.polyRing.T1]
  implicit def convert3(x: Map[Int, cuerpo.baseField.FpElement]): cuerpo.polyRing.T2 = cuerpo.polyRing.builder(x.asInstanceOf[cuerpo.polyRing.T1])

  /*implicit def convert4(x: Map[Int, Int]): cuerpo.T1 = {
    val tmp1 = x.toList
    def convert5(x: Int): cuerpo.baseField.T2 = cuerpo.baseField.builder(x)
    val tmp2 = tmp1.map(x => (x._1, convert5(x._2))).asInstanceOf[cuerpo.T1]
    tmp2
  }*/





  val tres: cuerpo.baseField.T2 = cuerpo.baseField.builder(3)

  val dos: cuerpo.baseField.T2 = 2

  val tmp1: cuerpo.polyRing.T2 = cuerpo.polyRing.one

  val tmp4: cuerpo.polyRing.T1 = Map(2 -> tres).asInstanceOf[cuerpo.polyRing.T1]

  val tmp5: Map[Int, cuerpo.baseField.FpElement] = Map(4 -> tres)

  val tmp3: cuerpo.polyRing.T2 = cuerpo.polyRing.builder(tmp5)

  val poly1: cuerpo.T2 = cuerpo.builder(tmp3)

  val poly2: cuerpo.T2 = cuerpo.builder(Map(3 -> tres))

  val poly3: cuerpo.T2 = cuerpo.builder(Map(0 -> convert1(4), 1 -> dos, 2 -> convert1(1)))

  val poly4: cuerpo.T2 = cuerpo.builder(Map(0 -> convert1(1), 1 -> convert1(2), 2 -> convert1(3)))

  println(poly3)

  println(cuerpo.h.isIrreducible)

  println("poly3 = " + poly3)
  println("poly4 = " + poly4)

  println("Suma = " + (poly3 + poly4))
  println("Resta = " + poly3.minus(poly4))
  println("Producto = " + poly3 * poly4)
  println("División = " + (poly3 / poly4))







}