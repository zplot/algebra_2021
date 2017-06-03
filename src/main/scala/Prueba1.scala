package algebra






object Prueba1 {

  def apply(t: Set[Cycle]): Prueba1 = {

    println("t = " + t)

    val struc: List[Cycle] = List(Cycle(List(1,2,3)))
    println("aqui = " + struc)

    new Prueba1(struc)

  }

  def unapply(x: Prueba1): Option[Set[Cycle]] = Some(x.struc.toSet)



/*  def fromListOfCyclestoMap(lc: List[Cycle]): Map[Int, Int] = {

    lc match {
      case Nil => Map[Int, Int]()
      case x :: xs => x.toMap ++ fromListOfCyclestoMap(xs)

    }
  }*/

  val one = new Prueba1(List(Cycle(List(1))))

  /*def generar(generadores: Set[Prueba1]): Set[Prueba1] = {

    def bucleMultiple(listaAnterior: Set[Prueba1], generadores: Set[Prueba1]): Set[Prueba1] = {

      val listaNueva =
        for {
          g <- listaAnterior
          s <- generadores
        } yield g.multiply(s)
      if (listaAnterior != listaNueva) {
        bucleMultiple(listaNueva ++ listaAnterior, generadores)
      } else {
        listaAnterior
      }
    }
    val listaInicial = generadores + one
    bucleMultiple(listaInicial, generadores)
  }*/



}

class Prueba1(val struc: List[Cycle]) {

  val numCiclos: Int = struc.length

  //val cicloMasLargo: Int = struc.map(x => x.length).max

  val tmp1: List[Int] = struc.map(x => x.length)
  println("tmp1 = " + tmp1)
  val tmp2: Int = tmp1.max
  //val cicloMasLargo: Int = tmp2

 // val toMap: Map[Int, Int] = Prueba1.fromListOfCyclestoMap(struc)

  // lowest canonical order
 /* val lco: List[Cycle] = {

    def orden(cycle1: Cycle, cycle2: Cycle): Cycle = (cycle1.toList, cycle2.toList) match {
      case (List(), List()) => cycle1
      case (_, List()) => cycle1
      case (List(), _) => cycle2
      case (x, y) if x.length < y.length => cycle1
      case (x, y) if x.length > y.length => cycle2
      case (x :: _, y :: _) if x < y => cycle1
      case (x :: _, y :: _) if x > y => cycle2
      case _ => cycle1

    }
    struc.sortWith((cycle1, cycle2) => orden(cycle1, cycle2) == cycle1)
  }

  override def toString: String = if (lco == List()) "1" else "P" + lco.mkString("")

  def inver: Prueba1 = {

    if (cicloMasLargo < 2) Permutation.one
    // El inverso de 1 es 1
    val tmp1 = this.lco
    val tmp2 = tmp1.map(x => x.cycle)
    val tmp3 = tmp2.map(x => x.reverse)
    val tmp4 = tmp3.map(x => Cycle(x))
    val tmp5 = tmp4.toSet
    Prueba1(tmp5)
  }

  def multiply(other: Prueba1): Prueba1 = (this, other) match {

    case (Prueba1.one, Prueba1.one) => Prueba1.one
    case (x, Prueba1.one) => x
    case (Prueba1.one, x) => x
    case (_, _) =>

      val izda: List[Cycle] = this.struc
      val dcha: List[Cycle] = other.struc
      val tmp1: List[Cycle] = izda ++ dcha
      val seMueven: List[Int] = tmp1.flatMap(x => x.cycle).distinct.sorted
      val mapaProducto: Map[Int, Int] =
        seMueven.foldRight[Map[Int, Int]](Map[Int, Int]())((i, mapa) =>
          mapa ++ Map(i -> this.toMap.getOrElse(other.toMap.getOrElse(i, i), other.toMap.getOrElse(i, i))))

      def concatenar(mapa: Map[Int, Int], lista: List[Int]): List[Int] = {
        if (!mapa.contains(lista.last)) {
          lista.dropRight(1)
        } else {
          concatenar(mapa - lista.last, lista :+ mapa(lista.last))
        }
      }

      def extinguir(mapa: Map[Int, Int], quedan: List[Int], ciclos: List[List[Int]]): List[List[Int]] = {

        if (quedan == List()) {
          ciclos
        } else {
          val cicloSiguiente = concatenar(mapa, List(quedan.head))
          val resultado1 = extinguir(mapa, quedan diff ciclos.flatten, ciclos ++ List(cicloSiguiente)).distinct
          resultado1
        }
      }


      val resultado1 = extinguir(mapaProducto, seMueven, List())
      val resultado2 = resultado1.map(x => Cycle(x)).toSet
      Prueba1(resultado2)
  }

  override def equals(o: Any): Boolean = o match {

    case that: Permutation => that.lco == this.lco
    case _ => false

  }

  override def hashCode: Int = lco.hashCode
*/


}

