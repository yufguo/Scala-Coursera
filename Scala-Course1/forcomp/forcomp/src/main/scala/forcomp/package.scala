package object forcomp {
  val dictionaryPath = List("forcomp", "linuxwords.txt")

  def loadDictionary = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
  
  def sqrtSeq(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x/guess)/2
    val guesses: Stream[Double] = 1 #:: (Stream[Double](1,2) map improve)
    guesses
  }
  
  sqrtSeq(4).take(10).toList

}
