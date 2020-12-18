package algebra2
import Utils.{coprime, divisors, haveCommonDivisors}

class Zn(m: Int) extends Ring {

  type S = Int
  type T = this.ZnElement

  val n: Int = m
  val zero: T = new ZnElement(0)
  val one: T = new ZnElement(1)
  def build(x: S): T = new ZnElement(x)
  override def toString: String = "Z" + m.toString
  val zeroDivisors: List[ZnElement] = divisors(n).filter(_ > 1).map(x => new ZnElement(x))
  val units: List[ZnElement] = {
    val tmp1 = List.range(1, n).filter(coprime(_, n)).map(x => new ZnElement(x))
    tmp1
  }
  val units2: List[ZnElement] = {
    val tmp1 = List.range(1, n + 1)
    val tmp2 = tmp1.filter(coprime(_, n)).map(x => new ZnElement(x))
    tmp2
  }


  class ZnElement(x: Int) extends Element {
    val value: Int = if (x % n < 0) x + n else x
    def add(other: T): T = new ZnElement((this.value + other.value) % n)
    def minus(other: T): T = new ZnElement((this.value + other.value) % n)
    def multiply(other: T): T = new ZnElement((this.value * other.value) % n)

    override def toString: String = value.toString
  }

}


