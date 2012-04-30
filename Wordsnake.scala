import collection.mutable.ListBuffer

object Wordsnake extends App {

  val allWords = args.toSet

  var allSeeds = allWords.toList.combinations(2).map {
    f: List[String] =>
      val a = f.head
      val b = f.tail.head
      Result(Set(a, b), merge(a, b))
  }.toSeq

  var done = false
  while (!done) {
    allSeeds = allSeeds.flatMap(_.advance)
    done = allSeeds.head.words == allWords
  }
  var shortSolutions = allSeeds.map(_.best)
  allSeeds = null
  var best = null: String
  while (!shortSolutions.isEmpty) {
    val (intermediate, soln) = shortSolutions.splitAt(10000)
    val iReduced = intermediate.par.reduce((a, b) => if (a.length < b.length) a else b)
    shortSolutions = soln
    if (best == null) {
      best = iReduced
    } else {
      if (intermediate.length < best.length) {
        best = iReduced
        println(best + " (" + best.length + ")")
      }
    }
  }
  print(best)

  case class Result(words: Set[String], solutions: Set[String]) {
    def advance: Iterable[Result] = {
      for {
        word <- allWords if !words.contains(word)
        b <- solutions
      } yield copy(words + word, merge(word, b))
    }
    def best: String = {
      solutions.reduce((a, b) => if (a.length < b.length) a else b)
    }
  }

  def merge(a: String, b: String): Set[String] = {
    if (a.length > b.length) {
      merge(b, a)
    } else {
      val buffer = new ListBuffer[String]
      buffer += a+b
      buffer += b+a
      if (b.contains(a)) {
        buffer += b
      }
      for (i <- 1 until a.length) {
        if (a.drop(a.length - i) == b.take(i)) {
          buffer += a.take(a.length - i) + b
        }
        if (a.take(i) == b.drop(b.length - i)) {
          buffer += b + a.drop(i)
        }
      }
      buffer.toSet
    }
  }
}
