package testing_algebra
import algebra2.Fp



object testFp extends App {
  println("Empezamos")

  val cuerpo = Fp(13)
  println(cuerpo)
  val cuatro = cuerpo.FpElement(4)
  val nueve = cuerpo.FpElement(9)
  val c14 = cuerpo.FpElement(14)

  println(cuerpo.one)
  println(c14)
  println(c14 == cuerpo.one)



}