package algebra2

trait Ring {

  type S // Seed
  type T // Element

  val zero: Element
  val one: Element

  def build(x: S): T

  trait Element {

    def add(other: T): T
    def +(other: T): T = this.add(other)
    def minus(other:T): T
    def -(other: T): T = this.minus(other)
    def multiply(other: T): T
    def *(other: T): T = this.multiply(other)
    def inverse: T

  }

}

