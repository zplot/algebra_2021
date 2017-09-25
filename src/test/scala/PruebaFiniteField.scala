package algebra

import algebra.PruebaFiniteField.cuerpo

import scala.language.implicitConversions
import algebra.Utils._



object PruebaFiniteField  extends App {
  println("Empezamos")

  val cuerpo = FiniteField(3,2)


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

  implicit def convert5(x: Int): cuerpo.polyRing.T0 = cuerpo.polyRing.field.builder(x)



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

  // println(cuerpo.h.isIrreducible)

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

  val cuatro: cuerpo.polyRing.T0 = cuerpo.polyRing.field.builder(4)

  val vector7: Vector[cuerpo.polyRing.T0] = Vector(3, 5, 6, 2)

  val poly7: cuerpo.T2 = cuerpo.builder(vector7)

  println(poly7)

  println("Sección 3")

  val todos = cuerpo.elements

  println("todos = " + todos)

  println("poly7 = " + poly7)
  println("poly7 is primitive = " + cuerpo.isPrimitiveElement(poly7))

  val primis = cuerpo.elements.filter(x => cuerpo.isPrimitiveElement(x))
  println("primis = " + primis)
  println("número de primis = " + primis.length)
  println("número de elementos = " + cuerpo.elements.length)

  val z1 = cuerpo.elements
  val z2 = z1.map(x => x.power(cuerpo.numElements) - x)
  val z3 = z2.forall(x => x == cuerpo.zero)

  println("z3 = " + z3)


  println("Sección 4")

  def ecuacion(x: cuerpo.FiniteFieldElement, y: cuerpo.FiniteFieldElement): cuerpo.FiniteFieldElement = x.power(2) + y.power(2) - cuerpo.one

  val step1 = for (i <- cuerpo.elements; j <- cuerpo.elements) yield {
    println("i = " + i)
    println("j = " + j)
    println("i^2 = " + i.power(2))
    println("j^2 = " + j.power(2))
    println("i^2 + j^2 = " + (i.power(2) + j.power(2)))
    println("ecuacion(i, j) == cuerpo.zero = " + (ecuacion(i, j) == cuerpo.zero))

    if (ecuacion(i, j) == cuerpo.zero) 1 else 0
  }

  println("Número de soluciones = " + step1.filter(x => x == 1).length)

  println("Sección 5")

  val cuerpoFp = FiniteField(7,1)


  implicit def qconvert1(x: Int): cuerpoFp.baseField.T2 = cuerpoFp.baseField.builder(x)
  implicit def qconvert2(x: Map[Int,cuerpoFp.baseField.T2]): cuerpoFp.polyRing.T1 = x.asInstanceOf[cuerpoFp.polyRing.T1]
  implicit def qconvert3(x: Map[Int, cuerpoFp.baseField.FpElement]): cuerpoFp.polyRing.T2 = cuerpoFp.polyRing.builder(x.asInstanceOf[cuerpoFp.polyRing.T1])


  implicit def qconvert4(x: Map[Int, Int]): cuerpoFp.T2 = {
    val tmp1: List[(Int, Int)] = x.toList
    val tmp2: List[(Int, cuerpoFp.baseField.FpElement)] = tmp1.map(x => (x._1, qconvert1(x._2)))
    val tmp3 = tmp2.toMap
    val tmp4: cuerpoFp.polyRing.T2 = cuerpoFp.polyRing.builder(tmp3)
    val tmp5: cuerpoFp.T2 = cuerpoFp.builder(tmp4)
    tmp5
  }

  implicit def qconvert5(x: Int): cuerpoFp.polyRing.T0 = cuerpoFp.polyRing.field.builder(x)



  val e1: cuerpo.T2 = Map(0 -> 1)
  val e2: cuerpo.T2 = Map(0 -> 2)
  val e3: cuerpo.T2 = Map(0 -> 3)
  val e4: cuerpo.T2 = Map(0 -> 4)
  val e5: cuerpo.T2 = Map(0 -> 5)
  val e6: cuerpo.T2 = Map(0 -> 6)

  println(e2 + e3)
  println(e2 * e3)
  println(e2 / e3)
  println(e2.inverse)










}