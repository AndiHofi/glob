import org.scalatest._

import scala.collection.immutable._
import GlobMatcher._
import GlobMatcher.{
  Empty => E,
  FixedSequence => FS,
  FixedSequencePrefix => WFS,
  AnyFixedLength => ?,
  Anything => *
}

class GlobTest extends FlatSpec with prop.TableDrivenPropertyChecks with Matchers {
  "GlobParser" should "parse AB" in {
    p("AB") should equal (FixedSequence("AB") :: Nil)
  }

  it should "parse empty pattern" in {
    p("") should equal (Empty :: Nil)
  }

  it should "parse *" in {
    p("*") should equal (Anything :: Nil)
  }

  it should "parse normalized patterns" in {
    val data = Table(
      "pattern" -> "result",
      "" -> List(E),
      "*" -> List(*),
      "AB" -> List(FS("AB")),
      "*AB" -> List(*, FS("AB")),
      "?AB" -> List(?(1), FS("AB")),
      "?*AB" -> List(?(1), *, FS("AB")),
      "AB*" -> List(FS("AB"), *),
      "AB?" -> List(FS("AB"), ?(1)),
      "AB?*" -> List(FS("AB"), ?(1), *),
      "AB*CD" -> List(FS("AB"), *, FS("CD")),
      "AB?CD" -> List(FS("AB"), ?(1), FS("CD")),
      "AB?*CD" -> List(FS("AB"), ?(1), *, FS("CD")),
      "???A???" -> List(?(3), FS("A"), ?(3)),
      "???*A???*" -> List(?(3), WFS("A"), ?(3), *),
      "A???B???C" -> List(FS("A"), ?(3), FS("B"), ?(3), FS("C")),
      "A*B*C" -> List(FS("A"), WFS("B"), *, FS("C")),
      "A???*B???*" -> List(FS("A"), ?(3), WFS("B"), ?(3), *),
      "*A????B*" -> List(BackTrackGroup(Seq(WFS("A"), ?(4), FS("B"))), *)
    )

    forAll(data) { (pattern, result) =>
      p(pattern) should equal (result)
    }
  }

  it should "normalize patterns" in {
    val data = Table(
      "patterns" -> "normalized",
      List("**", "***", "*" * 10) -> "*",
      List("*?", "*?*", "?**") -> "?*",
      List("A**", "A***") -> "A*",
      List("A?**", "A*?*", "A**?", "A*?") -> "A?*",
      List("A??*", "A??**", "A?*?", "A*??") -> "A??*",
      List("**A", "***A") -> "*A",
      List("?*A", "*?A", "**?A", "*?*A") -> "?*A",
      List("A*B", "A**B", "A***B") -> "A*B",
      List("A??*B", "A?*?B", "A*??*B", "A**??B") -> "A??*B",
      List("A") -> "A"
    )

    forAll(data) { (patterns, normalized) =>
      for (pt <- patterns) {
        p(pt).mkString("") should equal (normalized)
      }
    }
  }

  it should "match Strings" in {
    val data = Table(
      "pattern" -> "testString",
      "AB" -> "AB",
      "" -> "",
      "*" -> "",
      "*" -> "AB",
      "*" -> "blablub",
      "?" -> "A",
      "?" -> "B",
      "*AB" -> "AB",
      "*AB" -> "AAB",
      "*AB" -> "BAB",
      "*AB" -> "ABAB",
      "AB*" -> "AB",
      "AB*" -> "ABB",
      "AB*" -> "ABAB",
      "AB?" -> "ABA",
      "AB?" -> "AB?",
      "AB??" -> "ABAB",
      "AB????" -> "ABXYUV",
      "?AB?" -> "XABX",
      "?AB?*" -> "XABXAB",
      "AB*AB" -> "ABAB",
      "AB*AB" -> "ABXAB",
      "AB*AB" -> "ABXXXAB",
      "AB*AB" -> "ABAXXBAAAAAAXAAAAB",
      "XY*AB" -> "XYABABAB",
      "XY*AB?" -> "XYABABc",
      "XY*AB??" -> "XYABABAB",
      "AB*CD*EF*GH" -> "ABCDEFGH",
      "AB*CD*EF*GH" -> "AB--CDCDEFEFGHGH",
      "AB*CD*EF?GH" -> "ABCDEF-GH",
      "AB*CD?EF?GH" -> "ABCDCDCDCDCD-EF-GH",
      "AB*CD?EF?GH" -> "AB--CDCDCDCDCD-EF-GH",
      "AB*CD?EF?GH" -> "ABCDCDCDCD--CDxEF-GH",
      "A????B" -> "A----B",
      "*A????B" -> "A----B",
      "*A????B" -> "++A----B",
      "*A????B" -> "+AA----B",
      "A????B*" -> "A----B",
      "A????B*" -> "A----B++",
      "A????B*" -> "A----BB+",
      "*A????B*" -> "A----B",
      "*A????B*" -> "++A----B++",
      "*A????B*" -> "++A----BB+",
      "*A????B*" -> "+AA----B++",
      "*A????B*" -> "+AA----BB+"
      
    )

    forAll(data) {(pattern, testString) =>
      val matcher = GlobMatcher(pattern)
      matcher.matches(testString) should be (true)
    }
  }

  "FixedSequencePrefix" should "match exactly" in {
    val v = WFS("AB")

    v.matchNext("AB") should be (2)
  }

  "indexOf" should "work with startIndex 0" in {
    val data = Table(
      ("sourceString", "toFind", "expectedIndex"),
      ("", 'a', -1),
      ("a", 'a', 0),
      ("ba", 'a', 1),
      ("bb", 'a', -1),
      ("b" * 500, 'a', -1),
      ("b" * 500 + "a", 'a', 500),
      ("a" * 100, 'a', 0)
    )

    forAll(data) {(sourceString, toFind, expectedIndex) =>
      GlobMatcher.indexOf(sourceString, toFind) should be (expectedIndex)
    }
  }

  it should "work with every index up to expectedIndex" in {
    val data = Table(
      ("sourceString", "toFind", "expectedIndex"),
      ("a", 'a', 0),
      ("ba", 'a', 1),
      ("b"*50 + "a", 'a', 50)
    )

    forAll(data) {(sourceString, toFind, expectedIndex) =>
      for(startIndex <- 0 to expectedIndex) {
        GlobMatcher.indexOf(sourceString, toFind, startIndex) should be (expectedIndex)
      }
    }
  }

  "doesMatch" should "find matches" in {
    val data = Table(
      ("src", "srcI", "dst", "dstI", "length"),
      ("abcde", 0, "abcde", 0, 5)
    )

    forAll(data) {(src, srcI, dst, dstI, length) =>
      GlobMatcher.doesMatch(src, srcI, dst, dstI, length) should be (true)
    }
  }

  "BackTrackGroup" should "match when not backtracking" in {
    val matcher = BackTrackGroup(Array(WFS("ABC"), ?(2), FS("CDEFG")))

    matcher.matchNext("+++ABC--CDEFG") should be (13)
  }

  it should "backtrack when second FixedSequence does not match" in {
    val matcher = BackTrackGroup(Array(WFS("AB"), ?(2), FS("CD")))
    val data = Table[String](("input"),
      ("+++AB--AB--CD"),
      ("+++ABAB--CD"),
      ("+++AAB--CD"),
      ("AAAAAB--CD"),
      ("AB---DABAB--AB--CAB--CD"))

    forAll(data) {(input) =>
      matcher.matchNext(input) should be (input.length)
    }
  }

  def p(pattern: String) = GlobMatcher(pattern).matcherTree
}
