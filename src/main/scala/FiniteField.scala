package algebra
import algebra.Utils._

import scala.collection.immutable
import scala.language.implicitConversions


case class FiniteField(p: Int, w: Int) extends Field {
  require(isPrime(p), p + " is not a prime number")



  val numElements: Int = Utils.power(p, w)
  val baseField: Fp = Fp(p)
  val polyRing: PolynomialsOverFp = PolynomialsOverFp(baseField)
  val h: polyRing.T2 = polyRing.findConwayPol(w)
  val identity: FiniteFieldElement = builder(polyRing.one)

  type T1 = polyRing.Polynomial // Un polinomio sobre el cuerpo base
  type T2 = FiniteFieldElement


  // takes a polynomial in polyRing and builds a FiniteFieldElement
  def builder(x: T1): T2 = FiniteFieldElement(x)

  // takes a vector and builds a FiniteFieldElement
  def builder(x: Vector[polyRing.T0]): T2 = FiniteFieldElement(x)


  val structureId: String = "Fq(" + Utils.power(p, w).toString + ")"
  val finite: Boolean = true
  val zero: FiniteFieldElement = builder(polyRing.zero)
  val one: FiniteFieldElement = identity

  override def toString = structureId


  object FiniteFieldElement {

    def apply(f: T1): FiniteFieldElement = {
      val g: T1 = f.mod(h)
      new FiniteFieldElement(g)
    }

    def apply(v: Vector[polyRing.T0]): FiniteFieldElement = {
      val step0 = 0 to w -1
      val step1 = step0 zip v
      val step2: Map[Int, polyRing.T0] = step1.toMap
      val step3 = polyRing.builder(step2)
      val step4: T1 = step3.mod(h)
      new FiniteFieldElement(step4)
    }
  }


  def isPrimitiveElement(x: T2): Boolean = {
    val divisores: List[Int] = Utils.divisors(numElements - 1)
    val divisoresBien = divisores.drop(1).reverse.drop(1).reverse
    val step1: List[Boolean] = divisoresBien.map(k => x.power(k) != one)
    step1.forall( x => x == true)
  }


  def elements: List[FiniteFieldElement] = {
    def combinations(size: Int) : List[List[polyRing.T0]] = {
      if (size == 0)
        List(List())
      else {
        for {
          x  <- (0 to p -1).toList.map(x => polyRing.field.builder(x))
          xs <- combinations(size-1)
        } yield x :: xs
      }
    }
    val step1 = combinations(w)
    val step2 = step1.map(x => x.toVector)
    val step3 = step2.map(x => builder(x))
    step3
  }



  class FiniteFieldElement private(val f: T1) extends FieldElement {

    val fatherFiniteField: FiniteField = FiniteField.this
    val elementId: String = f.toString
    val exponents: Set[Int] = f.map.keySet
    val coefficients: Set[polyRing.T0] = f.map.values.toSet
    val isZero: Boolean = {
      val cond0: Boolean = this == zero
      val cond1: Boolean = coefficients.toList.forall(x => x == polyRing.field.zero)
      cond0 || cond1
    }

    val vector: Vector[polyRing.T0]  = {
      val tmp0: List[Int] = (0 to w - 1).toList
      def fromExpToCoef(x: Int): polyRing.T0 = if (this.f.map.contains(x)) this.f.map(x) else polyRing.field.zero
      val tmp1: List[polyRing.T0] = tmp0.map(x => fromExpToCoef(x))
      val tmp2 = tmp1.toVector
      tmp2
    }

    def add(other: T2): FiniteFieldElement = builder((f + other.f).mod(h))

    def minus(other: T2): FiniteFieldElement = builder((f - other.f).mod(h))

    def negate: T2 = builder((h - f).mod(h))

    def multiply(other: T2): FiniteFieldElement = builder((f * other.f).mod(h))

    def power(p: Int): T2 = p match {
      case 0 => one
      case 1 => this
      case p if p % 2 == 1 => this * (this * this).power((p - 1) / 2)
      case p if p % 2 == 0 => (this * this).power(p / 2)
    }

    def inverse: T2 = {
      if (this == zero) {
        throw new IllegalArgumentException("zero does not have inverse")
      } else {
        this.power(numElements - 2)
      }
    }

    //override def toString = "(" + "(" + f.toString + ") mod h)"
    override def toString: String = "(" + "(" + f.toString + ") mod "+ h.toString + ")"

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[FiniteFieldElement]
      if (that == null) false
      else this.f == that.f
    }
  }

}




