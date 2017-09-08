package algebra


object PruebaFp extends App {

  println("Empezamos")



  val cuerpo = Fp(7)

  implicit def imp1(x: Int): cuerpo.T2 = cuerpo.builder(x)

  val a = 5
  val b = 6


  println(a.minus(b))
  println(a.multiply(b))
  println(a.inverse*a)

  println(cuerpo.gcd(a, b))


}