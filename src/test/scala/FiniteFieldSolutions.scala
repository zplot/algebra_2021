package algebra

import algebra.PruebaFiniteField.cuerpo

import scala.language.implicitConversions
import algebra.Utils.IntMap



object FiniteFieldSolutions  extends App {
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


  type T = cuerpo.T2

  def equation(x: T, y: T): T = x.power(3) + y.power(2) - cuerpo.one



}