package algebra2

trait Field extends Ring {

  type S // Seed
  type T // FieldElement

  def inverse(x: T): T

  trait FieldElement extends RingElement {

    def divide(other: T): T

  }
}
