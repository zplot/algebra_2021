package algebra

import algebra.Utils.IntMap

import scala.io.{BufferedSource, Source}

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

  val p = 2
  val n = 5

  def pleidafun(s: String): Int = {
    val comaPos: Int = s.indexOf(",")
    val pleida = s.substring(1, comaPos)
    pleida.toInt
  }

  def nleidafun(s: String): Int = {
    val comaPos: Int = s.indexOf(",")
    val segundaComa: Int = s.indexOf(",", comaPos + 1)
    val nleida = s.substring(comaPos + 1, segundaComa)
    nleida.toInt
  }


  val bufferedSource: BufferedSource = Source.fromFile("allConwayPolynomials.txt")


  val tmp2: List[String] = bufferedSource.getLines.filter(s => pleidafun(s) == p && nleidafun(s) == n).toList

  bufferedSource.close

  val tmp3: String = tmp2.head

  println(tmp3)

  val corchetePos1: Int = tmp3.indexOf("[", 2)
  println("corchetePos1 = " + corchetePos1)

  val corchetePos2: Int = tmp3.indexOf("]")
  println("corchetePos2 = " + corchetePos2)

  val tmp4 = tmp3.substring(corchetePos1 + 1, corchetePos2)
  println(tmp4)

  val tmp5: List[Int] = tmp4.split(",").map(_.toInt).toList
  println(tmp5)

  val cosa1 = anillo.findConwayPol(5)
  println("cuerpo = " + anillo.field)
  println(cosa1)

  println("Empezamos2")



  val poly11 = anillo.builder(Map(0 -> 6, 1 -> 4, 2 -> 5))
  val poly12 = anillo.builder(Map(0 -> 1, 1 -> 2))

  println("poly11 = " + poly11)
  println("poly12 = " + poly12)


  val cociente1 = poly11/poly12


  println("poly11/poly12 = " + cociente1)











}