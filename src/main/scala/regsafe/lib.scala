package regsafe

import java.util.regex.{Pattern, Matcher}
import scala.collection.AbstractIterator
import scala.util.matching.Regex.MatchData

// Based on 2.13.6's implementation of scala.util.matching.Regex

class Regex[P] private[regsafe](val pattern: Pattern, groupNames: String*) extends Serializable { outer =>

  import Regex._

  def this(regex: String, groupNames: String*) =
    this(Pattern.compile(regex), groupNames: _*)

  def unapply(s: CharSequence)(implicit n: Sanitizer[P]): Option[P] =
    val m = pattern.matcher(s)
    if (runMatcher(m)) Some(extractGroupsFromMatch(m))
    else None

  def unapply(m: Match)(implicit n: Sanitizer[P]): Option[P] =
    if (m.matched == null) None
    else if (m.matcher.pattern == this.pattern) Some(extractGroupsFromMatch(m.matcher))
    else unapply(m.matched)

  def unapplySeq(c: Char): Option[List[Char]] =
    val m = pattern matcher c.toString
    if (runMatcher(m))
      if (m.groupCount > 0) Some((m group 1).toList) else Some(Nil)
    else None

  protected def runMatcher(m: Matcher): Boolean = m.matches()

  def findAllIn(source: CharSequence): MatchIterator =
    new Regex.MatchIterator(source, this, groupNames)

  def findAllMatchIn(source: CharSequence): Iterator[Match] =
    val matchIterator = findAllIn(source)
    new AbstractIterator[Match] {
      def hasNext = matchIterator.hasNext
      def next(): Match =
        matchIterator.next()
        new Match(matchIterator.source, matchIterator.matcher, matchIterator.groupNames).force
    }

  def findFirstIn(source: CharSequence): Option[String] =
    val m = pattern.matcher(source)
    if (m.find) Some(m.group) else None

  def findFirstMatchIn(source: CharSequence): Option[Match] =
    val m = pattern.matcher(source)
    if (m.find) Some(new Match(source, m, groupNames)) else None

  def findPrefixOf(source: CharSequence): Option[String] =
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(m.group) else None

  def findPrefixMatchOf(source: CharSequence): Option[Match] =
    val m = pattern.matcher(source)
    if (m.lookingAt) Some(new Match(source, m, groupNames)) else None

  def matches(source: CharSequence): Boolean =
    runMatcher(pattern.matcher(source))

  def replaceAllIn(target: CharSequence, replacement: String): String =
    val m = pattern.matcher(target)
    m.replaceAll(replacement)

  def replaceAllIn(target: CharSequence, replacer: Match => String): String =
    val it = new Regex.MatchIterator(target, this, groupNames).replacementData
    it foreach (md => it replace replacer(md))
    it.replaced

  def replaceSomeIn(target: CharSequence, replacer: Match => Option[String]): String =
    val it = new Regex.MatchIterator(target, this, groupNames).replacementData
    for (matchdata <- it ; replacement <- replacer(matchdata))
      it replace replacement
    it.replaced

  def replaceFirstIn(target: CharSequence, replacement: String): String =
    val m = pattern.matcher(target)
    m.replaceFirst(replacement)

  def split(toSplit: CharSequence): Array[String] =
    pattern.split(toSplit)

  def unanchored: UnanchoredRegex[P] =
    new Regex[P](pattern, groupNames: _*) with UnanchoredRegex[P] {
      override def anchored = outer
    }

  def anchored: Regex[P] = this

  def regex: String = pattern.pattern

  override def toString: String = regex
}

trait UnanchoredRegex[P] extends Regex[P] {
  override protected def runMatcher(m: Matcher): Boolean = m.find()
  override def unanchored: UnanchoredRegex[P] = this
}

object Regex {
  def apply[R <: String & Singleton](regex: R): Regex[Compile[R]] =
    new Regex(regex)

  private def extractGroupsFromMatch[P](m: Matcher)(implicit n: Sanitizer[P]): P =
    val arr = Array.tabulate[Any](m.groupCount)(i => m.group(i + 1))
    assert(arr.size == n.i, "Unexpected number of capturing groups")
    n.mutate(arr)
    if (arr.size == 1)
      arr(0).asInstanceOf[P]
    else
      Tuple.fromArray(arr).asInstanceOf[P]


  abstract class Sanitizer[T](val i: Int) {
    def mutate(arr: Array[Any]): Unit
  }

  object Sanitizer {
    implicit val basecase: Sanitizer[EmptyTuple] =
      new Sanitizer[EmptyTuple](0) {
        def mutate(arr: Array[Any]): Unit = ()
      }

    implicit def stringcase[T <: Tuple]
      (implicit ev: Sanitizer[T]): Sanitizer[String *: T] =
        new Sanitizer[String *: T](ev.i + 1) {
          def mutate(arr: Array[Any]): Unit =
            assert(arr(arr.size - i) != null)
            ev.mutate(arr)
        }

    implicit def optioncase[T <: Tuple]
      (implicit ev: Sanitizer[T]): Sanitizer[Option[String] *: T] =
        new Sanitizer[Option[String] *: T](ev.i + 1) {
          def mutate(arr: Array[Any]): Unit =
            arr(arr.size - i) = Option(arr(arr.size - i))
            ev.mutate(arr)
        }

    implicit val tuple1stringcase: Sanitizer[String] =
      new Sanitizer[String](1) {
        def mutate(arr: Array[Any]): Unit =
          assert(arr(0) != null)
      }

    implicit val tuple1optioncase: Sanitizer[Option[String]] =
      new Sanitizer[Option[String]](1) {
        def mutate(arr: Array[Any]): Unit =
          arr(0) = Option(arr(0))
      }
  }

  class Match(val source: CharSequence,
              protected[regsafe] val matcher: Matcher,
              val groupNames: Seq[String]) extends MatchData {

    val start: Int = matcher.start

    val end: Int = matcher.end

    def groupCount: Int = matcher.groupCount

    private[this] lazy val starts: Array[Int] =
      Array.tabulate(groupCount + 1) { matcher.start }

    private[this] lazy val ends: Array[Int] =
      Array.tabulate(groupCount + 1) { matcher.end }

    def start(i: Int): Int = starts(i)

    def end(i: Int): Int = ends(i)

    def force: this.type = { starts; ends; this }
  }

  object Match {
    def unapply(m: Match): Some[String] = Some(m.matched)
  }

  object Groups {
    def unapplySeq(m: Match): Option[Seq[String]] =
      if (m.groupCount > 0)
        Some(List.tabulate(m.groupCount) { i => m.group(i + 1) })
      else
        None
  }

  class MatchIterator(val source: CharSequence, val regex: Regex[_], val groupNames: Seq[String]) extends AbstractIterator[String] with Iterator[String] with MatchData {
    self =>

    protected[Regex] val matcher = regex.pattern.matcher(source)
    private[this] var nextSeen = 0

    def hasNext: Boolean =
      nextSeen match
        case 0 => nextSeen = if (matcher.find()) 1 else 3
        case 1 => ()
        case 2 => nextSeen = 0 ; hasNext
        case 3 => ()
      nextSeen == 1

    def next(): String =
      nextSeen match
        case 0 => if (!hasNext) throw new NoSuchElementException ; next()
        case 1 => nextSeen = 2
        case 2 => nextSeen = 0 ; next()
        case 3 => throw new NoSuchElementException
      matcher.group

    override def toString: String = super[AbstractIterator].toString

    private[this] def ensure(): Unit = nextSeen match
      case 0 => if (!hasNext) throw new IllegalStateException
      case 1 => ()
      case 2 => ()
      case 3 => throw new IllegalStateException

    def start: Int = { ensure() ; matcher.start }

    def start(i: Int): Int = { ensure() ; matcher.start(i) }

    def end: Int = { ensure() ; matcher.end }

    def end(i: Int): Int = { ensure() ; matcher.end(i) }

    def groupCount: Int = { ensure() ; matcher.groupCount }

    def matchData: Iterator[Match] =
      new AbstractIterator[Match] {
        def hasNext = self.hasNext
        def next() = { self.next(); new Match(source, matcher, groupNames).force }
      }

    def replacementData =
      new AbstractIterator[Match] with Replacement {
        def matcher = self.matcher
        def hasNext = self.hasNext
        def next() = { self.next(); new Match(source, matcher, groupNames).force }
      }
  }

  private[regsafe] trait Replacement {
    protected def matcher: Matcher

    private[this] val sb = new java.lang.StringBuffer

    def replaced = {
      val newsb = new java.lang.StringBuffer(sb)
      matcher.appendTail(newsb)
      newsb.toString
    }

    def replace(rs: String) = matcher.appendReplacement(sb, rs)
  }

  def quote(text: String): String = Pattern quote text
  def quoteReplacement(text: String): String = Matcher quoteReplacement text

  import compiletime.ops.string.{Substring, Length, Matches}
  import compiletime.ops.int.{+, -, Max}

  type CharAt[R <: String, At <: Int] =
    Substring[R, At, At + 1]

  type Compile[R <: String] =
    Matches["", R] match
      case _ => Reverse[Loop[R, 0, Length[R], EmptyTuple, IsPiped[R, 0, Length[R], 0]]]

  type Loop[R <: String, Lo <: Int, Hi <: Int, Acc <: Tuple, Opt <: Int] <: Tuple =
    Lo match
      case Hi => Acc
      case _  => CharAt[R, Lo] match
        case "\\" => CharAt[R, Lo + 1] match
          case "Q" => Loop[R, ToClosingQE[R, Lo + 2], Hi, Acc, Opt]
          case _ => Loop[R, Lo + 2, Hi, Acc, Opt]
        case "[" => Loop[R, ToClosingBracket[R, Lo + 1, 0], Hi, Acc, Opt]
        case ")" => Loop[R, Lo + 1, Hi, Acc, Max[0, Opt - 1]]
        case "(" => Opt match
          case 0 => IsMarked[R, ToClosingParenthesis[R, Lo + 1, 0], Hi] match
            case true => IsCapturing[R, Lo + 1] match
              case false => Loop[R, Lo + 1, Hi, Acc, 1]
              case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, 1]
            case false => IsCapturing[R, Lo + 1] match
              case false => Loop[R, Lo + 1, Hi, Acc, IsPiped[R, Lo + 1, Hi, 0]]
              case true  => Loop[R, Lo + 1, Hi, String *: Acc, IsPiped[R, Lo + 1, Hi, 0]]
          case _ => IsCapturing[R, Lo + 1] match
            case false => Loop[R, Lo + 1, Hi, Acc, Opt + 1]
            case true  => Loop[R, Lo + 1, Hi, Option[String] *: Acc, Opt + 1]
        case _ => Loop[R, Lo + 1, Hi, Acc, Opt]

  type IsCapturing[R <: String, At <: Int] <: Boolean =
    CharAt[R, At] match
      case "?" => CharAt[R, At + 1] match
        case "<" => CharAt[R, At + 2] match
          case "=" | "!" => false
          case _ => true
        case _ => false
      case _ => true

  type IsMarked[R <: String, At <: Int, Hi <: Int] <: Boolean =
    At match
      case Hi => false
      case _ => CharAt[R, At] match
        case "?" | "*" => true
        case "{" => CharAt[R, At + 1] match
          case "0" => true
          case _ => false
        case _ => false

  type IsPiped[R <: String, At <: Int, Hi <: Int, Lvl <: Int] <: Int =
    At match
      case Hi => 0
      case _ => CharAt[R, At] match
        case "\\" => CharAt[R, At + 1] match
          case "Q" => IsPiped[R, ToClosingQE[R, At + 2], Hi, Lvl]
          case _ => IsPiped[R, At + 2, Hi, Lvl]
        case "[" => IsPiped[R, ToClosingBracket[R, At + 1, 0], Hi, Lvl]
        case "(" => IsPiped[R, ToClosingParenthesis[R, At + 1, 0], Hi, Lvl + 1]
        case "|" => 1
        case ")" => 0
        case _ => IsPiped[R, At + 1, Hi, Lvl]

  type ToClosingParenthesis[R <: String, At <: Int, Lvl <: Int] <: Int =
    CharAt[R, At] match
      case "\\" => CharAt[R, At + 1] match
        case "Q" => ToClosingParenthesis[R, ToClosingQE[R, At + 2], Lvl]
        case _ => ToClosingParenthesis[R, At + 2, Lvl]
      case "[" => ToClosingParenthesis[R, ToClosingBracket[R, At + 1, 0], Lvl]
      case ")" => Lvl match
        case 0 => At + 1
        case _ => ToClosingParenthesis[R, At + 1, Lvl - 1]
      case "(" => ToClosingParenthesis[R, At + 1, Lvl + 1]
      case _   => ToClosingParenthesis[R, At + 1, Lvl]

  type ToClosingBracket[R <: String, At <: Int, Lvl <: Int] <: Int =
    CharAt[R, At] match
      case "\\" => CharAt[R, At + 1] match
        case "Q" => ToClosingBracket[R, ToClosingQE[R, At + 2], Lvl]
        case _ => ToClosingBracket[R, At + 2, Lvl]
      case "[" => ToClosingBracket[R, At + 1, Lvl + 1]
      case "]" => Lvl match
        case 0 => At + 1
        case _ => ToClosingBracket[R, At + 1, Lvl - 1]
      case _ => ToClosingBracket[R, At + 1, Lvl]

  type ToClosingQE[R <: String, At <: Int] <: Int =
    CharAt[R, At] match
      case "\\" => CharAt[R, At + 1] match
        case "E" => At + 2
        case _ => ToClosingQE[R, At + 2]
      case _ => ToClosingQE[R, At + 1]

  // Scala's unapply does not play well with Tuple1, so we need to get ride of
  // those types. Furthermore, the code responsible for generating _n
  // extractors for tuples does not know about generic tuples, so we also
  // manually flattern tuple types here...
  type Reverse[L <: Tuple] = L match
    case EmptyTuple => Unit
    case x1 *: EmptyTuple => x1
    case x1 *: x2 *: EmptyTuple => (x2, x1)
    case x1 *: x2 *: x3 *: EmptyTuple => (x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: EmptyTuple => (x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: EmptyTuple => (x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: EmptyTuple => (x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: EmptyTuple => (x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: EmptyTuple => (x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: EmptyTuple => (x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: EmptyTuple => (x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: EmptyTuple => (x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: EmptyTuple => (x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: EmptyTuple => (x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: EmptyTuple => (x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: EmptyTuple => (x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: EmptyTuple => (x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: EmptyTuple => (x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: EmptyTuple => (x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: EmptyTuple => (x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: x20 *: EmptyTuple => (x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: x20 *: x21 *: EmptyTuple => (x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
    case x1 *: x2 *: x3 *: x4 *: x5 *: x6 *: x7 *: x8 *: x9 *: x10 *: x11 *: x12 *: x13 *: x14 *: x15 *: x16 *: x17 *: x18 *: x19 *: x20 *: x21 *: x22 *: EmptyTuple => (x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1)
}
