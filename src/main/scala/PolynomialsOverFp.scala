package algebra

import algebra.Utils._
import scala.io.{BufferedSource, Source}
import scala.language.implicitConversions

/**
  * builder of a Polynomial Ring over Fp
  */
object PolynomialsOverFp {

  def apply(field: Fp): PolynomialsOverFp = {
    new PolynomialsOverFp(field)
  }
}

class PolynomialsOverFp private(val field: Fp)  {


  type T0 = field.FpElement
  type T1 = Map[Int, T0]
  type T2 = Polynomial

  // takes a Map[Int, T0] and builds a Polynomial
  def builder(x: T1): T2 = Polynomial(x)

  def findAllIrredPol(degree: Int): List[T2] = {

    def fromListToPoly(list: List[field.FpElement]): T2 = {
      val oneTwoThree = (0 to degree).toList
      val ourZip = oneTwoThree zip list
      val ourMap = ourZip.toMap
      builder(ourMap)
    }

    val listOfFieldElements = (0 until field.p).map(field.builder).toList
    val listOflists = combinations[field.FpElement](degree + 1, listOfFieldElements)
    val listOflistsDrop0 = listOflists.tail
    val listOfPolys = listOflistsDrop0 map (x => fromListToPoly(x))
    val listMonic = listOfPolys.filter(_.isMonic)
    val listOfPolysDegree = listMonic.filter(_.degree == degree)
    val listOfIrreducibles = listOfPolysDegree.filter(_.isIrreducible)

    listOfIrreducibles
  }

  def fromListToPol(coefs: List[T0]): Polynomial = {
    val lista = 0.to(coefs.length - 1)
    val tmp1 = for (i <- lista) yield (i, coefs(i))
    val tmp2 = tmp1.toMap
    val tmp3 = builder(tmp2)
    tmp3
  }

  def findConwayPol(n: Int): Polynomial = {

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
    val tmp2: List[String] = bufferedSource.getLines.filter(s => pleidafun(s) == field.p && nleidafun(s) == n).toList
    bufferedSource.close

    val tmp3: String = tmp2.head
    val corchetePos1: Int = tmp3.indexOf("[", 2)
    val corchetePos2: Int = tmp3.indexOf("]")
    val tmp4 = tmp3.substring(corchetePos1 + 1, corchetePos2)
    val tmp5: List[Int] = tmp4.split(",").map(_.toInt).toList
    val tmp6 = tmp5.map(x => field.builder(x))
    val poly1 = fromListToPol(tmp6)
    poly1
  }



  // See Victor Shoup pag. 472
  def gcdExtended(g: T2, h: T2): (T2, T2, T2) = {

    val r: T2 = g
    val rPrime: T2 = h
    val s: T2 = builder(Map(0 -> field.one))
    val sPrime: T2 = zero
    val t: T2 = zero
    val tPrime: T2 = builder(Map(0 -> field.one))

    def loop(r: T2, s: T2, t: T2, rPrime: T2, sPrime: T2,tPrime: T2): (T2, T2, T2,T2, T2, T2) = {

      if (rPrime != zero) {
        val (q, rPrimePrime) = r / rPrime
        loop(rPrime, sPrime, tPrime, rPrimePrime, s - sPrime * q, t - tPrime * q)
      } else {
        val c = r.lc
        val (d, tmp2) = r / c
        val sNew = s / c
        val tNew = t / c
        (d, sNew._1, tNew._1, zero, zero, zero)
      }
    }
    val (gcdFinal,sFinal,tFinal, dummy1, dummy2, dummy3) = loop(r, s, t, rPrime, sPrime, tPrime)
    (gcdFinal, sFinal, tFinal)
  }

  def gcd(g: T2, h: T2): T2 = {
    val tmp = gcdExtended(g,h)
    tmp._1
  }

  // TODO Hacer esto con el Squering algorithm
  def exp(h: T2, exponent: Int): T2 = {

    def loop(h: T2, exp: Int, acc: T2): T2 = {
      if (exp <= 1) acc else loop(h, exp - 1, acc * h)
    }

    loop (h, exponent, h)
  }

  /**
    * builder of a Polynomial belonging to the polynomial ring
    */
  object Polynomial {

    val fatherPolynomialOverFp: PolynomialsOverFp = PolynomialsOverFp.this

    def apply(map: T1): T2 = {

      val normalMap: T1 = {

        val theMapList = map.toList.filter(x => x._1 != 0 || x._2 != field.zero)
        def newMapList(oldMapList: List[(Int, T0)]): List[(Int, T0)] = oldMapList match {
          case Nil => Nil

          case ((0, field.zero) :: Nil) => (0, field.zero) :: Nil
          case ((x1, field.zero) :: xs) => newMapList(xs)
          case ((x1, x2) :: xs)  => (x1, x2) :: newMapList(xs)
        }
        val theNewMapList = newMapList(theMapList)
        val theNewMap = theNewMapList.toMap
        theNewMap
      }
      new Polynomial(normalMap)
    }

    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, T0), monomial2: (Int, T0)): Boolean = monomial1._1 > monomial2._1

  }

  val zero: T2 = builder(Map(0 -> field.zero))

  val x: T2 = builder(Map(1 -> field.one))

  val one: T2 = builder(Map(0 -> field.one))



  class Polynomial private(val map: T1)  {

    def add(other: T2): T2 = {

      val tmp3 = field.zero
      val exponents = (map.keySet ++ other.map.keySet).toList
      def recursion(exp: List[Int]): Map[Int, T0] = exp match {
        case Nil => Map[Int, T0]()
        case x :: xs => recursion(xs) + (x -> (map.getOrElse(x, tmp3) + other.map.getOrElse(x, tmp3)))
      }

      val temporalMap = recursion(exponents)
      val temporalPoly = builder(temporalMap)
      val result = temporalPoly
      result
    }

    def minus(other: T2): T2 = this.add(other * field.builder(field.p - 1))

    def multiply(other: T2): T2 = {

      val step1: List[(Int, T0)] = for (i <- this.map.toList; j <- other.map.toList) yield (i._1 + j._1, i._2 * j._2)
      val exponents: List[Int] = step1.map(x => x._1).distinct
      val step2: List[List[(Int, T0)]] = for (i <- exponents) yield step1.filter(x => x._1 == i)

      def sumListInRing(list1: List[T0]): T0 = list1 match {
        case Nil => field.zero
        case x :: xs => x + sumListInRing(xs)
      }

      def sumCoef(l: List[(Int, T0)]): (Int, T0) = {
        (l.head._1, sumListInRing(l.map(x => x._2)))
      }

      val step3 = step2.map(sumCoef)
      val step4 = step3.toMap
      builder(step4)

    }

    val degree: Int = {
      //val step1 =  map.keySet
      val step1 = map.keySet
      if (this.map == Map(0 -> field.zero) || this.map == Map[Int, T0]()) -1 else step1.max
    }

    val lc: T0 = {
      if (degree == -1) field.zero else this.map(degree)
    }

    val isMonic: Boolean = lc == field.one

    type Monomial = (Int, T0)

    val lt: Monomial = if (degree == -1) (0, field.zero) else (degree, lc)

    def monomialDivision(dividendo: Monomial, divisor: Monomial): Polynomial = {
      builder(Map(dividendo._1 - divisor._1 -> dividendo._2.divide(divisor._2)))
    }

    def divide(other: T2): (T2, T2) = {

      val dividendoOrigen = this
      val divisor = other
      val cocienteAcum = zero

      def loop(dividendo: Polynomial, divisor: Polynomial, cocienteAcum: Polynomial): ( Polynomial, Polynomial) = {

        if (dividendo.degree < divisor.degree) (cocienteAcum, dividendo) else {
          val nuevoCocienteAcum: Polynomial = cocienteAcum + monomialDivision(dividendo.lt, divisor.lt)
          val tmp1 = divisor * nuevoCocienteAcum
          val nuevoDividendo: Polynomial = dividendoOrigen - tmp1
          loop(nuevoDividendo, divisor, nuevoCocienteAcum)

        }
      }



      loop(dividendoOrigen, divisor, cocienteAcum)
    }

    def divide(other: T0): (T2, T2) = {

      val a: T2 = this
      val b: T2 = builder(Map(0 -> other))
      a.divide(b)

    }

    def multiply(other: T0): T2 = {
      val tmp1 = builder(Map(0 -> other))
      this.multiply(tmp1)
    }

    def *(other: T2): T2 = this.multiply(other)
    def *(other: T0): T2 = this.multiply(other)

    def +(other: T2): T2 = this.add(other)

    def -(other: T2): T2 = this.minus(other)

    def /(other: T2): (T2, T2) = this.divide(other)
    def /(other: T0): (T2, T2) = this.divide(other)

    def toMonic: T2 = {
      val oldMapList = map.toList
      def newMapList(oldMapList: List[(Int, T0)]): List[(Int, T0)] = oldMapList match {
        case Nil => Nil
        case ((x1, x2) :: xs)  => (x1, x2 / lc) :: newMapList(xs)
      }
      if (lc == field.one) this else {
        val finalMap = newMapList(oldMapList).toMap
        new T2(finalMap)
      }
    }

    def mod(h: T2): T2 = this.divide(h)._2

    //  Cohen page 127
    def isIrreducible: Boolean = {

      val n = degree
      val cond1: Boolean = {

        val exponent = math.pow(field.p, degree).toInt
        val xToPn = exp(x, exponent)
        //xToPn.mod(this) == x.mod(this)
        val xToPnMinusX = xToPn - x
        val tmp2 = xToPnMinusX.mod(this) == zero
        tmp2
      }

      val cond2: Boolean = {
        val factores = factors(n)
        val factors2 = factores.slice(1, factores.length - 1)
        val tmp1 = for(q <- factors2) yield {
          val exp1 = n/q
          val tmp2 = exp(x, exp1)
          val tmp2MinusX = tmp2 - x
          val tmp4 = gcd(tmp2MinusX, this)
          val tmp5 = tmp4 == one
          tmp5
        }
        tmp1.forall(x => x)
      }

      cond1 && cond2
    }

    override def toString: String = {
      def printPol(a: List[(Int, T0)]): String = a match {
        case Nil => ""
        case x :: xs if x._1 == 0 => x._2 + " + " + printPol(xs)
        case x :: xs if x._1 == 1 && x._2 == field.one => "x" + " + " + printPol(xs)
        case x :: xs if x._1 == 1 => x._2 + "x" + " + " + printPol(xs)
        case x :: xs if x._2 == field.one => "x" + x._1 + " + " + printPol(xs)
        case x :: xs => x._2 + "x" + x._1 + " + " + printPol(xs)
      }
      if (this == zero) "0" else printPol(this.map.toList.sortWith(Polynomial.comp)).dropRight(3)
      //map.toString()
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T2]
      if (that == null) false
      else {
        val a1: Boolean = this.map == that.map
        val a2: Boolean = (this.map == Map[Int, T0]()) && (that.map == Map(0 -> field.zero))
        val a3: Boolean = (that.map == Map[Int, T0]()) && (this.map == Map(0 -> field.zero))
        a1 || a2 || a3
      }
    }

  }
}

