package algebra2

case class Fp(n: Int) extends Field {

  type S = Int
  type T = this.FpElement

  def build(x: S): T = FpElement(x)

  val zero: T = FpElement(0)
  val one: T = FpElement(1)

  def inverse(x: T): T = ???

  case class FpElement(x: Int) extends FieldElement {

    def divide(x: T): T = ???
    def add(other: FpElement): FpElement = ???
    def minus(other: FpElement): FpElement = ???
    def multiply(other: FpElement): FpElement = ???
  }

}
