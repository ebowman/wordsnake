import akka.dispatch.{Await, Future, ExecutionContext}
import akka.util.Duration
import annotation.tailrec
import java.util.concurrent.{TimeUnit, Executors}

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 5/2/12 10:02 AM
 */

object Heuristic extends App {

  val start = System.currentTimeMillis
  val allWords = if (args.isEmpty) {
      "subway dentist wayward highway terrible english less blessed warden rib stash shunt hunter ".split("\\s+").toSet
    } else {
      args.toSet
    }
  val threads = 8
  val executor = Executors.newFixedThreadPool(threads)
  implicit val ctx = ExecutionContext.fromExecutor(executor)

  val subsize = 9
  val futures = for (_ <- 1 to threads) yield {
    val seed = util.Random.shuffle(allWords.toList)
    println(seed)
    Future(("seed = %s, subsize = %d".format(seed, subsize), heuristic(seed, subsize)))
  }

  implicit val lenOrdering = new Ordering[String] {
    def compare(x: String, y: String) = x.length - y.length
  }

  val result = futures.map(Await.result(_, Duration.Inf))
  val best = result.map(_._2).min
  val seed = result.find(_._2 == best).get
  println("%s (%d) (from %s)".format(best, best.length, seed._1))
  executor.shutdown()
  executor.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
  println("Execution time: %d ms".format(System.currentTimeMillis - start))

  @tailrec
  private def heuristic(words: List[String], subsize: Int): String = {
    val nowWords = words.take(subsize)
    val restWords = words.drop(subsize)
    val solver = new Wordsnake(nowWords, false)
    val best = solver.solve(0)
    if (restWords.isEmpty) {
      best
    } else {
      heuristic(best :: restWords, subsize)
    }
  }
}
