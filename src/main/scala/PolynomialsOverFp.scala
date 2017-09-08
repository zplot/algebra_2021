package algebra
import algebra.Utils._

/**
  * builder of a Polynomial Ring over field
  */
object PolynomialsOverFp {

  def apply(field: Fp): PolynomialsOverFp = {
    new PolynomialsOverFp(field)
  }
}

class PolynomialsOverFp private(val field: Fp)  {

  type T1 = Map[Int, Int]
  type T2 = Polynomial

  // takes a Map[Int, Int] and builds a Polynomial
  def builder(x: Map[Int, Int]): T2 = Polynomial(x)

  def findAllIrredPol(degree: Int): List[T2] = {

    def fromListToPoly(list: List[Int]): T2 = {
      val oneTwoThree = (0 to degree).toList
      val ourZip = oneTwoThree zip list
      val ourMap = ourZip.toMap
      builder(ourMap)
    }

    val listOfFieldElements = (0 until field.p).toList
    val listOflists = combinations[Int](degree + 1, listOfFieldElements)
    val listOflistsDrop0 = listOflists.tail
    val listOfPolys = listOflistsDrop0 map (x => fromListToPoly(x))
    val listMonic = listOfPolys.filter(_.isMonic)
    val listOfPolysDegree = listMonic.filter(_.degree == degree)
    val listOfIrreducibles = listOfPolysDegree.filter(_.isIrreducible)

    listOfIrreducibles
  }

  def findIrredPol(degree: Int): T2 = findAllIrredPol(degree).head

  def findIrredPolProb(degree: Int): T2 = {

    def fromListToPolynomial(list: List[Int]) = {
      val listOfElements = list
      val oneTwoThree = list.indices.toList.reverse
      val quasiMap = oneTwoThree zip listOfElements
      val map = quasiMap.toList.toMap
      builder(map)
    }

    val p = field.p

    def genPolynomial = {
      val sequence = for (i <- 1 to degree) yield randomP(p)
      val list = sequence.toList
      val listExtended = 1 :: list
      val polynomial = fromListToPolynomial(listExtended)
      polynomial
    }

    def loop: T2 = {
      val generated = genPolynomial
      if (generated.isIrreducible) {
        generated
      } else {
        loop
      }
    }

    loop
  }


  // See Victor Shoup pag. 472
  def gcdExtended(g: T2, h: T2): (T2, T2, T2) = {

    val r: T2 = g
    val rPrime: T2 = h
    val s: T2 = builder(Map(0 -> 1))
    val sPrime: T2 = zero
    val t: T2 = zero
    val tPrime: T2 = builder(Map(0 -> 1))

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
        val theMapList = map.toList
        def newMapList(oldMapList: List[(Int, Int)]): List[(Int, Int)] = oldMapList match {
          case Nil => Nil
          case ((x1, 0) :: xs) => newMapList(xs)
          case ((x1, x2) :: xs)  => (x1, x2) :: newMapList(xs)
        }
        val theNewMapList = newMapList(theMapList)
        val theNewMap = theNewMapList.toMap
        theNewMap
      }
      new Polynomial(normalMap)
    }

    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, Int), monomial2: (Int, Int)): Boolean = monomial1._1 > monomial2._1

  }

  val zero: T2 = builder(Map(0 -> 0))

  val x: T2 = builder(Map(1 -> 1))

  val one: T2 = builder(Map(0 -> 1))



  class Polynomial private(val map: T1)  {

    def add(other: T2): T2 = {

      val tmp3 = 0
      val exponents = (map.keySet ++ other.map.keySet).toList
      def recursion(exp: List[Int]): Map[Int, Int] = exp match {
        case Nil => Map[Int, Int]()
        case x :: xs => recursion(xs) + (x -> (map.getOrElse(x, tmp3) + other.map.getOrElse(x, tmp3)))
      }

      val temporalMap = recursion(exponents)
      val temporalPoly = builder(temporalMap)
      val result = temporalPoly
      result
    }

    def minus(other: T2): T2 = {
      val tmp1 = other
      val tmp2 = -1
      val tmp3 = tmp1 * tmp2
      val tmp4 = this.add(tmp3)
      tmp4
    }

    def multiply(other: T2): T2 = {
      val step1 = for (i <- this.map.toList; j <- other.map.toList) yield (i._1 + j._1, i._2 * j._2)
      val exponents = step1.map(x => x._1).distinct
      val step2 = for (i <- exponents) yield step1.filter(x => x._1 == i)

      def sumListInRing(list1: List[Int]): Int = list1 match {
        case Nil => 0
        case x :: xs => x + sumListInRing(xs)
      }

      def sumCoef(l: List[(Int, Int)]): (Int, Int) = {
        (l.head._1, sumListInRing(l.map(x => x._2)))
      }

      val step3 = step2.map(sumCoef)
      val step4 = step3.toMap
      builder(step4)

    }



    val degree: Int = {
      //val step1 =  map.keySet
      val step1 = map.keySet
      if (this.map == Map(0 -> 0) || this.map == Map[Int, Int]()) -1 else step1.max
    }

    val lc: Int = {
      if (degree == -1) 0 else this.map(degree)
    }

    val isMonic: Boolean = lc == 1

    // Ver https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclid.27s_algorithm
    def divide(other: T2): (T2, T2) = {

      val a: T2 = this
      val b: T2 = other
      val d: Int = b.degree



      def rem(a: T2, b: T2): T2 = {

        var q: T2 = zero
        var r: T2 = a
        val d: Int = b.degree
        var s: T2 = zero

        while (r.degree >= d) {
          println("r.lc = " + r.lc)
          println("b.lc = " + b.lc)
          println("r.lc / b.lc  = " + r.lc / b.lc)

          s = builder(Map( r.degree - d -> r.lc / b.lc))
          q = q + s
          r = r - s * b
          println("r = " + r)
          println("d = " + d)
          println("s(r) = " + s)
        }
        r
      }

      def loop(q: T2, r: T2): (T2, T2) = {
        if (r == zero) (q, r) else {

          println("r = " + r)
          println("rem(q, r) = " + rem(q, r))
          loop(r, rem(q, r))
        }
      }
      loop(a, b)
    }

    def divide(other: Int): (T2, T2) = {

      val a: T2 = this
      val b: T2 = builder(Map(0 -> other))
      a.divide(b)

    }

    def multiply(other: Int): T2 = this.multiply(builder(Map(0 -> other)))

    def *(other: T2): T2 = this.multiply(other)
    def *(other: Int): T2 = this.multiply(other)

    def +(other: T2): T2 = this.add(other)

    def -(other: T2): T2 = this.minus(other)

    def /(other: T2): (T2, T2) = this.divide(other)
    def /(other: Int): (T2, T2) = this.divide(other)

    def toMonic: T2 = {
      val oldMapList = map.toList
      def newMapList(oldMapList: List[(Int, Int)]): List[(Int, Int)] = oldMapList match {
        case Nil => Nil
        case ((x1, x2) :: xs)  => (x1, x2 % lc) :: newMapList(xs)
      }
      if (lc == 1) this else {
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
      def printPol(a: List[(Int, Int)]): String = a match {
        case Nil => ""
        case x :: xs if x._1 == 0 => x._2 + " + " + printPol(xs)
        case x :: xs if x._1 == 1 && x._2 == 1 => "x" + " + " + printPol(xs)
        case x :: xs if x._1 == 1 => x._2 + "x" + " + " + printPol(xs)
        case x :: xs if x._2 == 1 => "x" + x._1 + " + " + printPol(xs)
        case x :: xs => x._2 + "x" + x._1 + " + " + printPol(xs)
        //"hola hola 189"
      }
      if (this == zero) "0" else printPol(this.map.toList.sortWith(Polynomial.comp)).dropRight(3)
      //map.toString()
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T2]
      if (that == null) false
      else {
        val a1: Boolean = this.map == that.map
        val a2: Boolean = (this.map == Map[Int, Int]()) && (that.map == Map(0 -> 0))
        val a3: Boolean = (that.map == Map[Int, Int]()) && (this.map == Map(0 -> 0))
        a1 || a2 || a3
      }
    }

  }
}

