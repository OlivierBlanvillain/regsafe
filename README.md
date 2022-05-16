## Type-Safe Regular Expressions

Regsafe is a regular expression library which provides a type-safe and null-safe mechanism for capturing group extraction.
The implementation makes extensive use of [match types](https://docs.scala-lang.org/scala3/reference/new-types/match-types.html) to statically analyze regular expressions during type checking.
The Regsafe API mimics Scala's original regex APIs and is intended to be usable as a drop-in replacement.

The original implementation of this library is thoroughly documented in a [Scala Symposium paper](sigplan.pdf).

## The Goodies

### Type safety

There is a bug in the following code, can you spot it?

```scala
import regsafe.Regex

val date = Regex("""(\d{4})-(\d{2})-(\d{2})""")
"2004-01-20" match
  case date(y, m, d, a) =>
    s"$y was a good year for PLs."
```

With Regsafe, this is a compilation error:

```
[error] -- Error: src/test/scala/regsafe/neg.scala:9:14
[error] 9 |  case date(y, m, d, a) =>
[error]   |            ^
[error]   |this case is unreachable since type (String, String, String)
[error]   |is not a subclass of class Tuple4
```

The library derives the result type of `regsafe.Regex.unapply` by doing a type-level analysis of the regex literal singleton type.
In this case, after counting parenthesis, the implementation concludes that `"(\d{4})-(\d{2})-(\d{2})"` has exactly 3 capturing groups and returns a `(String, String, String)`, which is much more descriptive than the `Seq[String]` type used in the standard library's implementation.

### Null safety

Regsafe's type-level analysis also identifies nullable capturing groups. The `unapply` method nicely wraps those groups in options for null-safe consumption:

```scala
import regsafe.Regex

val rational = Regex("""(\d+)(?:\.(\d+))?""")
"3.1415" match
  case rational(i, Some(f)) =>
    val n = i.size + f.size
    s"This number is $n digits long"
```

### Regex validation

Regsafe detect syntactically invalid regular expressions at compile time: (error messages are identical to those emitted by the JVM's runtime)

```
[error] 12 |  Regex("(")
[error]    |       ^
[error]    |       Unclosed group near index 1

[error] 13 |  Regex(")")
[error]    |       ^
[error]    |       Unmatched closing ')'

[error] 14 |  Regex("\\")
[error]    |       ^
[error]    |       Unexpected internal error near index 1
```
