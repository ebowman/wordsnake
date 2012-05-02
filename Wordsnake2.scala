import java.util.Date

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 5/2/12 2:47 PM
 */

object Driver extends App {
  val start = System.currentTimeMillis
  val snake = new Wordsnake2
  val words = "subway dentist wayward highway rib terrible english blessed less warden stash shunt hunter".split("\\s+").toList
  val redundant = (for (a <- words; b <- words if a != b) yield if (a.contains(b)) Some(b) else None).flatten.distinct
  val reduced = words filterNot (redundant contains)
  val result = snake.solve(reduced)
  val stop = System.currentTimeMillis
  println(result.toList.sorted.mkString("\n"))
  println("Total time = %s".format(Format.formatMs(stop - start)))
}

final class Wordsnake2 {
  def solve(words: List[String]): Set[String] = {
    var count = 0
    val pageSize = 10000 // how many permutations to deal with in one go
    val permutationCount = (2l to words.size.toLong).foldLeft(1l)(_ * _)
    println("%s permutations".format(permutationCount))
    var bestSolutions = List(words.mkString)
    val permutationIterator = words.permutations
    val start = System.currentTimeMillis
    while (permutationIterator.hasNext) {
      val nextPage = permutationIterator.take(pageSize).toSeq//.par // seems to be buggy
      count += 1
      printProgress(count)
      val candidates = nextPage.fold(bestSolutions) {
        (previousSolutions, permutation) =>
          val newSolution = permutation.reduce(mergeFast)
          val bestScore = previousSolutions.head.length
          if (newSolution.length < bestScore) {
            List(newSolution)
          } else if (newSolution.length == bestScore && !previousSolutions.contains(newSolution)) {
            newSolution :: previousSolutions
          } else {
            previousSolutions
          }
      }
      if (candidates.head.length < bestSolutions.head.length) {
        bestSolutions = candidates
      } else if (candidates.head.length == bestSolutions.head.length) {
        bestSolutions = (candidates ::: bestSolutions).distinct
      }
    }

    def printProgress(count: Int) {
      val now = System.currentTimeMillis
      val percent = (0d + pageSize * count) / permutationCount
      val tookMs = now - start
      val msPer = (tookMs + 0d) / (pageSize * count)
      val msLeft = (1d - percent) * permutationCount * msPer
      if (((pageSize * count) % 10000000) == 0) {
        println("done: %s (%2.2f%%) (%s remaining) (%s) (ms per permutation: %2.5f)".format(
          pageSize * count, 100d * percent, Format.formatMs(math.round(msLeft)), new Date(now + math.round(msLeft)).toString, msPer))
      }
    }
    bestSolutions.toSet
  }

  def merge(a: String, b: String): String = {
    val right = mergeFast(a, b)
    val wrong = merge2(a, b)
    if (right != wrong) {
      merge2(a, b)
    }
    right
  }

  def merge2(a: String, b: String): String = {
    if (b.contains(a)) {
      b
    } else {
      for (i <- 1 until a.length) {
        if (b.startsWith(a.drop(i))) {
          return a.take(i) + b
        }
      }
      a + b
    }
  }

  def mergeFast(a: String, b: String): String = {
    val aArr = a.toCharArray
    val bArr = b.toCharArray
    val resultBuffer = new Array[Char](a.length + b.length)
    var i = 1
    val limit = aArr.length
    while (i < limit) {
      var matches = true
      var j = 0
      val limit2 = math.min(aArr.length - i, bArr.length)
      while (matches && j < limit2 && matches) {
        if (aArr(i + j) != bArr(j)) {
          matches = false
        }
        j += 1
      }
      if (matches) {
        if (i + bArr.length < aArr.length) {
          return a
        } else {
          System.arraycopy(aArr, 0, resultBuffer, 0, i)
          System.arraycopy(bArr, 0, resultBuffer, i, bArr.length)
          return new String(resultBuffer, 0, i + bArr.length)
        }
      }
      i += 1
    }
    if (a.length >= b.length && a.indexOf(b, 0) > -1) {
      a
    } else if (b.indexOf(a, 0) > -1) {
      b
    } else {
      System.arraycopy(aArr, 0, resultBuffer, 0, aArr.length)
      System.arraycopy(bArr, 0, resultBuffer, aArr.length, bArr.length)
      new String(resultBuffer, 0, resultBuffer.length)
    }
  }
}

object Format {
  def formatMs(ms: Long): String = {
    val msPerDay = 1000 * 60 * 60 * 24
    val msPerHour = msPerDay / 24
    val msPerMinute = msPerHour / 60
    val msPerSecond = msPerMinute / 60

    val days = ms / msPerDay
    val hourMs = ms % msPerDay

    val hours = hourMs / msPerHour
    val minuteMs = hourMs % msPerHour

    val minutes = minuteMs / msPerMinute
    val secondMs = minuteMs % msPerMinute

    val seconds = secondMs / msPerSecond
    val msMs = secondMs % msPerSecond

    (days, hours, minutes, seconds, msMs) match {
      case h if h._1 == 0 =>
        "%02d:%02d:%02d.%d".format(h._2, h._3, h._4, h._5)
      case d =>
        "%s days %02d:%02d:%02d.%d".format(d._1, d._2, d._3, d._4, d._5)
    }
  }
}
