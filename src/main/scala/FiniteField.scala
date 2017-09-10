package algebra
import algebra.Utils._

/*


case class FiniteField(p: Int, w: Int) extends Field {
  require(isPrime(p), p + " is not a prime number")



  val numElements: Int = Utils.power(p, w)
  val baseField: Fp = Fp(p)
  val polyRing: PolynomialsOverFp = PolynomialsOverFp(baseField)
  val h: polyRing.T2 = polyRing.findIrredPolProb(w)
  val identity: FiniteFieldElement = builder(polyRing.one)

  type T1 = polyRing.T2 // Un polinomio sobre el cuerpo base
  type T2 = FiniteFieldElement


  // takes a polynomial in polyRing and builds a FiniteFieldElement
  def builder(x: T1): T2 = FiniteFieldElement(x)

  // takes a Map[Int, Int] and builds a FiniteFieldElement
  def builder(x: IntMap): T2 = {

    val xMap: Map[Int, Int] = x.map
    val xList: List[(Int, Int)] = xMap.toList
    val xList2 = xList
    val xMap2: Map[Int, Int] = xList2.toMap
    val polynomialInPolyRing = polyRing.builder(xMap2)
    FiniteFieldElement(polynomialInPolyRing)
  }

  val structureId: String = "Fq(" + Utils.power(p, w).toString + ")"
  val finite: Boolean = true
  val zero = builder(polyRing.zero)
  val one = identity

  override def toString = structureId


  object FiniteFieldElement {
    def apply(f: T1) = {
      val g: T1 = f.mod(h)
      new FiniteFieldElement(g)
    }
  }

  class FiniteFieldElement private(val f: T1) extends FieldElement {

    val fatherFiniteField: FiniteField = FiniteField.this
    val elementId: String = f.toString
    val exponents: Set[Int] = f.map.keySet
    val coefficients: Set[Int] = f.map.values.toSet
    val isZero: Boolean = {
      val cond0: Boolean = this == zero
      val cond1: Boolean = coefficients.toList.forall(x => x == 0)
      cond0 || cond1
    }

    def add(other: T2) = builder((f + other.f).mod(h))

    def minus(other: T2) = builder((f - other.f).mod(h))

    def negate: T2 = builder((h - f).mod(h))

    def multiply(other: T2) = builder((f * other.f).mod(h))

    def power(p: Int): T2 = p match {
      case 0 => one
      case 1 => this
      case p if p % 2 == 1 => this * (this * this).power((p - 1) / 2)
      case p if p % 2 == 0 => (this * this).power(p / 2)
      //case p if p % 2 == 1 => this * (this * this).power((p - 1) / 2)
      //case p if p % 2 == 0 => (this * this).power(p / 2)
    }

    def inverse: T2 = {
      if (this == zero) {
        throw new IllegalArgumentException("zero does not have inverse")
      } else {
        this.power(numElements - 2)
      }
    }

    override def toString = "(" + "(" + f.toString + ") mod h)"

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[FiniteFieldElement]
      if (that == null) false
      else this.f == that.f
    }
  }

}



*/
