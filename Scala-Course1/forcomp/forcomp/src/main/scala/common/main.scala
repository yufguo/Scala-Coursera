package common

object main extends App{
  def sqrtSeq(x: Double): Stream[Double] = {
   def improve(guess: Double) = {println(guess);(guess + x/guess)/2}
   def x() : Int = 1 + x()
   lazy val guesses : Stream[Double] = { 1 #:: (guesses map improve)}
   println(guesses)
   println(guesses)
   guesses
  }
  println(sqrtSeq(4).take(3).tail.tail)

  def sqrtSeq1(x: Double): List[Double] = {
   def improve(guess: Double) = {println(guess);(guess + x/guess)/2}
   def construct(x : Double, cnt : Int): List[Double] = {
     if(cnt > 0) x :: construct(improve(x), cnt -1)
     else Nil
   }
   construct(1.0, 4)
  }
  
  println(sqrtSeq1(4))
}

