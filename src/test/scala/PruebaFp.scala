package algebra


object PruebaFp extends App {

  println("Empezamos")



  val cuerpo: Fp = Fp(7)




  implicit def imp1(x: Int): cuerpo.T2 = cuerpo.builder(x)

  val a = 23453
  val b = 35754674


  println(a.minus(b))
  println(a.multiply(b))
  println(a.inverse*a)
  println(cuerpo.gcd(a, b))
  println("base = " + a)
  println("exponente = " + b)

  println(a.power(b))
  println(a.powerSinTailRecursion(b))


}