import java.util.Date

/**
 * Brute-force solves the "wordsnake" problem. Wordsnakes are the different ways one can combine
 * words together using as few letters as possible.  For example, "terrible" and "english" have two
 * (ordered) wordsnakes: "terribleenglish" and "terriblenglish".  For the wordsnakes problem, we take
 * a list of words and compute all the smallest ways to combine them.
 *
 * This is in essence a travelling salesman problem, which is ultimately a problem in permutations.
 * In this program we consider all permutations of the list of words, and fold over the permutation
 * a merge operation that finds the shortest wordsnake for each adjacent pair of words, so that we end
 * up with the smallest wordsnake for each permutation.  When then iterate over all permutations and
 * remember the best paths.
 *
 * One interesting aspect of this program from a scala point of view, is how it uses a lazy iterator over
 * the permutations, and a fold operation (instead of foldLeft).  The fold works over a parallel collection,
 * so we pull off some number of permutations from the iterator, and then do the parallel fold over them.
 * It took some experimentation to find the right value for "pageSize", the number to pull of the iterator
 * in one go.
 *
 * This program runs fastest without about a 500M heap, though will run fine (slightly slower) with a much
 * smaller heap; I ran it with a 5M heap to completion (though much slower than with a bigger heap).
 *
 * $ scala -cp target/scala-2.9.1/classes -J-Xms5m -J-Xmx5m -J-server Main
 *
 * This idea was taken from _Puzzles for Programmers and Pros_ by Dennis E. Sasha, Wiley Publishing, Indianapolis,
 * (c) pp.171-173
 *
 * @author Eric Bowman
 * @since 5/2/12 2:47 PM
 */

object Main extends App {
  val start = System.currentTimeMillis
  val snake = new SnakeSolver
  val words = "subway dentist wayward highway rib terrible english blessed less warden stash shunt hunter".split("\\s+").toList
  println("Wordsnakes for " + words.mkString("\"", "\", \"", "\""))
  val degenerate = (for (a <- words; b <- words if a != b) yield if (a.contains(b)) Some(b) else None).flatten.distinct
  if (!degenerate.isEmpty) println("Some degeneracies exist (%s); pruning".format(degenerate.mkString("\"", "\", \"", "\"")))
  val reduced = words filterNot (degenerate contains)
  val result = snake.solve(reduced)
  val stop = System.currentTimeMillis
  println(result.toList.sorted.mkString("\n"))
  println("Total time = %s".format(Format.formatMs(stop - start)))
}

final class SnakeSolver {
  def solve(words: List[String]): Set[String] = {
    var count = 0
    val pageSize = 10000 // how many permutations to deal with in one go. Tuned value to take advantage of parallelism.
    val longestWord = words.map(_.length).max
    val permutationCount = (2l to words.size.toLong).foldLeft(1l)(_ * _)
    println("%s permutations".format(permutationCount))
    var bestSolutions = List(words.mkString)
    val permutationIterator = words.permutations
    val start = System.currentTimeMillis
    while (permutationIterator.hasNext) {
      val nextPage = permutationIterator.take(pageSize).toSeq.par
      count += 1
      printProgress(count)
      val candidates = nextPage.fold(bestSolutions) {
        (previousSolutions, permutation) =>
          if (permutation.find(_.length > longestWord).isEmpty) {
            val newSolution = permutation.reduce(mergeFast)
            val bestScore = previousSolutions.head.length
            if (newSolution.length < bestScore) {
              List(newSolution)
            } else if (newSolution.length == bestScore && !previousSolutions.contains(newSolution)) {
              newSolution :: previousSolutions
            } else {
              previousSolutions
            }
          } else {
            if (previousSolutions.head.length < permutation.head.length) {
              previousSolutions
            } else if (previousSolutions.head.length == permutation.head.length) {
              (previousSolutions ::: permutation).distinct
            } else {
              permutation
            }
          }
      }
      if (candidates.head.length < bestSolutions.head.length) {
        bestSolutions = candidates
      } else if (candidates.head.length == bestSolutions.head.length) {
        bestSolutions = (candidates ::: bestSolutions).distinct
      }
    }

    def printProgress(count: Int) {
      if (((pageSize * count) % 10000000) == 0) {
        val now = System.currentTimeMillis
        val percent = (0d + pageSize * count) / permutationCount
        val tookMs = now - start
        val msPer = (tookMs + 0d) / (pageSize * count)
        val msLeft = (1d - percent) * permutationCount * msPer
        println("%s complete (%2.2f%%) (%s remaining) (%s) (ms per permutation: %2.5f)".format(
          pageSize * count, 100d * percent, Format.formatMs(math.round(msLeft)), new Date(now + math.round(msLeft)).toString, msPer))
      }
    }
    bestSolutions.toSet
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
    // because we removed degeneracies in advance, we don't need to check
    // whether b contains a here.  If we didn't remove the degeneracies,
    // we would need a check here like:
    //    if (b contains a) {
    //      return b
    //    }
    // The algorithm above catches the a contains b case already
    System.arraycopy(aArr, 0, resultBuffer, 0, aArr.length)
    System.arraycopy(bArr, 0, resultBuffer, aArr.length, bArr.length)
    new String(resultBuffer, 0, resultBuffer.length)
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
