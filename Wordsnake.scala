import collection.mutable.ListBuffer
import java.util.Date

class Wordsnake(allWordList: List[String], log: Boolean = true) {

  private val allWordSet = allWordList.toSet
  private var allSeeds = allWordList.combinations(2).map {
    f: List[String] =>
      val a = f.head
      val b = f.tail.head
      Result(Set(a, b), merge(a, b))
  }.toSeq

  private def minStr(a: String, b: String) = if (a.length < b.length) a else b

  def solve(desiredLength: Int): String = {
    if (log) println("Start: " + new Date)
    val startTime = System.currentTimeMillis
    while (allSeeds.head.words != allWordSet) {
      allSeeds = allSeeds.flatMap(_.advance)
    }
    var shortSolutions = allSeeds.map(_.best)
    allSeeds = null
    var best = allWordList.mkString
    while (!shortSolutions.isEmpty) {
      val (intermediate, soln) = shortSolutions.splitAt(10000)
      val reduced = intermediate.reduce(minStr)
      shortSolutions = soln
      if (reduced.length < best.length) {
        best = reduced
        if (log) println((System.currentTimeMillis - startTime) + ": " + new Date + ": " + best + " (" + best.length + ")")
        if (best.length <= desiredLength) {
          shortSolutions = Nil
        }
      }
    }
    best
  }

  private case class Result(words: Set[String], solutions: Set[String]) {
    def advance: Iterable[Result] = {
      for {
        word <- allWordList if !words.contains(word)
        b <- solutions
      } yield copy(words + word, merge(word, b))
    }
    def best: String = {
      solutions.reduce(minStr)
    }
  }

  private def merge(a: String, b: String): Set[String] = {
    if (a.length > b.length) {
      merge(b, a)
    } else {
      val buffer = new ListBuffer[String]
      buffer += a+b
      buffer += b+a
      if (b.contains(a)) {
        buffer += b
      }
      var i = 1
      while (i < a.length) {
        if (a.drop(a.length - i) == b.take(i)) {
          buffer += a.take(a.length - i) + b
        }
        if (a.take(i) == b.drop(b.length - i)) {
          buffer += b + a.drop(i)
        }
        i += 1
      }
      buffer.toSet
    }
  }
}
