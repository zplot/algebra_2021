package algebra2

trait Field extends Ring {

  def inverse(x: T): T

  trait FieldElement extends RingElement {

    def divide(other: T): T

  }
}
