/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 5/2/12 10:02 AM
 */

object Bruteforce extends App {
  val allWords = if (args.isEmpty) {
      "subway dentist wayward highway terrible english less blessed warden rib stash shunt hunter ".split("\\s+").toList
    } else {
      args.toList
    }
  val solver = new Wordsnake(allWords)
  println(solver.solve(50))
}
