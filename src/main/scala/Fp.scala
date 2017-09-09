package algebra
import algebra.Utils._

import scala.annotation.tailrec

case class Fp(p: Int) extends Field {
  require(isPrime(p), p + " is not a prime number")

  type T1 = Int
  type T2 = FpElement




  def builder(x: T1): T2 = FpElement(x)





  val identity: FpElement = builder(1) // TODO conviven identity con one?
  val structureId: String = "Fp" + p.toString
  val finite: Boolean = true
  val zero: FpElement = builder(0)

  override val one: FpElement = builder(1)
  val minusOne: FpElement = builder(p - 1)

  def modulo(x: T1): T1 = if (x < 0) -x else x

  private object FpElement {
    def apply(k: T1): FpElement = {
      val v: Int = if (k < 0) {
        (modulo(k) / p + 1) * p + k
      } else {
        k
      }
      new FpElement(v % p)
    }
  }
  class FpElement (val k: T1)  extends FieldElement {

    val elementId: String = k.toString
    val fatherFp: Fp = Fp.this
    val isZero: Boolean = k == 0

    def add(other: FpElement): FpElement = builder((k + other.k) % p)
    def minus(other: FpElement): FpElement = builder((k - other.k) % p)
    def negate: FpElement = builder(p - k)
    def multiply(other: FpElement): FpElement = builder((k * other.k) % p)


    def powerSinTailRecursion(p: Int): FpElement = p match {
      case 0 => one
      case 1 => this
      case q if q % 2 == 1 => this * (this * this).power((q - 1) / 2)
      case q if q % 2 == 0 => (this * this).power(q / 2)
    }

    def power(p: Int): FpElement = {
      @tailrec
      def exponen(factor: FpElement, exponente: Int, base: FpElement): FpElement = exponente match {
        case 0 => factor
        case 1 => factor * base
        case q if q % 2 == 1 => exponen(factor * base, (q - 1) / 2, base * base)
        case q if q % 2 == 0 => exponen(factor, q / 2, base * base)
      }
      exponen(one, p, this)
    }

    def inverse: T2 = {
      if (this == zero) {
        throw new IllegalArgumentException("zero does not have inverse")
      } else {
        this.power(p - 2)
      }
    }

    override def toString: String = k.toString // + " mod " + n.toString

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T2]
      if (that == null) false
      else (this.k % p) == (that.k % p)
    }
  }
}

