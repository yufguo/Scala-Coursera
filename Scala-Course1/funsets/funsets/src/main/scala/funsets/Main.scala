package funsets
import FunSets._


abstract class SemiGroup[A] {
  def add(x: A, y: A): A
}
abstract class Monoid[A] extends SemiGroup[A] {
  def unit: A
}

object Monoid {
  implicit object StringMonoid extends Monoid[String] {
    def add(x: String, y: String): String = x concat y
    def unit: String = ""
  }
}
object Main extends App {
  val test = List(1,-1,2,-2,3)
  println(test span (x => x > 0))
  


  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
    if (xs.isEmpty) m.unit
    else m.add(xs.head, sum(xs.tail))

  //println(sum(List(1, 2, 3)))       // uses IntMonoid implicitly
  println(sum(List("a", "b", "c"))) // uses StringMonoid implicitly
}
