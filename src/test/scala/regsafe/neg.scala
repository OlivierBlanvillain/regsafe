import regsafe._
import scala.annotation.experimental

object Foo {
  val date = Regex("""(\d{4})-(\d{2})-(\d{2})""")

  // "2004-01-20" match
  //   case date(y, m, d, a) =>
  //     s"$y was a good year for PLs."

  val rational = Regex("""(\d+)(?:\.(\d+))?""")

  "3.1415" match
    case rational(i, Some(f)) =>
      val n = i.size + f.size
      s"This number is $n digits long"

  // Regex("(")
  // Regex(")")
  // Regex("\\")
}
