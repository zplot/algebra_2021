package algebra
import algebra.Utils._
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

  // TODO
  def primitiveElement: T2 = ???

  // TODO
  def allElements: Iterable[T2] = ???

  //TODO
  def fromVectorToPoly(coordinates: Vector[baseField.T2]): T2 = ???



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
      println("Vector = " + tmp2)
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




