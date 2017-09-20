package algebra

import algebra.PruebaFiniteField.cuerpo

import scala.language.implicitConversions
import algebra.Utils.IntMap



object PruebaFiniteField  extends App {
  println("Empezamos")

  val cuerpo = FiniteField(7,3)


  implicit def convert1(x: Int): cuerpo.baseField.T2 = cuerpo.baseField.builder(x)
  implicit def convert2(x: Map[Int,cuerpo.baseField.T2]): cuerpo.polyRing.T1 = x.asInstanceOf[cuerpo.polyRing.T1]
  implicit def convert3(x: Map[Int, cuerpo.baseField.FpElement]): cuerpo.polyRing.T2 = cuerpo.polyRing.builder(x.asInstanceOf[cuerpo.polyRing.T1])


  implicit def convert4(x: Map[Int, Int]): cuerpo.T2 = {
    val tmp1: List[(Int, Int)] = x.toList
    val tmp2: List[(Int, cuerpo.baseField.FpElement)] = tmp1.map(x => (x._1, convert1(x._2)))
    val tmp3 = tmp2.toMap
    val tmp4: cuerpo.polyRing.T2 = cuerpo.polyRing.builder(tmp3)
    val tmp5: cuerpo.T2 = cuerpo.builder(tmp4)
    tmp5
  }



  val tres: cuerpo.baseField.T2 = cuerpo.baseField.builder(3)

  val dos: cuerpo.baseField.T2 = 2

  val tmp1: cuerpo.polyRing.T2 = cuerpo.polyRing.one

  val tmp4: cuerpo.polyRing.T1 = Map(2 -> tres).asInstanceOf[cuerpo.polyRing.T1]

  val tmp5: Map[Int, cuerpo.baseField.FpElement] = Map(4 -> tres)

  val tmp3: cuerpo.polyRing.T2 = cuerpo.polyRing.builder(tmp5)

  val poly1: cuerpo.T2 = cuerpo.builder(tmp3)

  val poly2: cuerpo.T2 = Map(3 -> 3)

  val poly3: cuerpo.T2 = Map(0 -> 6, 1 -> 4, 2 -> 5)

  val poly4: cuerpo.T2 = Map(0 -> 1, 1 -> 2, 2 -> 0)

  val poly4bis: cuerpo.T2 = Map(0 -> 1, 1 -> 2, 2 -> 0)

  val poly5: cuerpo.T2 = Map(3 -> 3)

  println(poly3)

  println(cuerpo.h.isIrreducible)

  println("poly3 = " + poly3)
  println("poly4 = " + poly4)

  println("Suma = " + (poly3 + poly4))
  println("Resta = " + poly3.minus(poly4))
  println("Producto = " + poly3 * poly4)
  println("División = " + (poly3 / poly4))
  println("Inverso de poly3 = " + poly3.inverse)
  println("Prueba del inverso de poly3 = " + poly3.inverse * poly3)
  println("Inverso de poly4 = " + poly4.inverse)
  println("Prueba del inverso de poly4 = " + poly4.inverse * poly4)
  println("Prueba de la división = " + (poly3 / poly4) * poly4)
  println("poly3 * inverso de poly4 = " + poly3 * poly4.inverse)
  println("Prueba de la división = " + poly4 * poly3 * poly4.inverse)
  println("Son iguales poly4 y poly4bis = " + (poly4 == poly4bis))

  println("Empezamos 2")

  val poly6: cuerpo.T2 = Map(0 -> 1, 1 -> 2, 2 -> 0)










}