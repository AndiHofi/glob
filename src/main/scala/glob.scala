import scala.annotation.tailrec

object GlobMatcher {

  def apply(pattern: String) = {
    val mpSeq:Seq[MP] = parseMP(pattern)
    val (prefix, suffix) = splitPrefixSuffix(mpSeq)


    val optimized = optimize(prefix, Seq(), Seq())


    new GlobMatcher((optimized ++ suffix).toArray, optimized.toArray, suffix.toArray, pattern)
  }

  @tailrec
  final def optimize(oldSeq: Seq[MP], newSeq: Seq[MP], buildUp: Seq[MP]): Seq[MP] = oldSeq match {
    case Seq() =>
      newSeq ++ cleanBuildUp(buildUp)

    case Seq(fs: FixedSequence, rest @ _*) =>
      optimize(rest, newSeq, buildUp :+ fs)

    case Seq(afl: AnyFixedLength, rest @ _*) =>
      optimize(rest, newSeq, buildUp :+ afl)

    case Seq(cur, rest @_*) =>
      optimize(rest, newSeq ++ cleanBuildUp(buildUp), Seq(cur))
  }

  def cleanBuildUp(mpSeq: Seq[MP]): Seq[MP] = mpSeq match {
    case a: Seq[MP] if a.length < 3 => a
    case a @ Seq(_: FixedSequence, _ @ _*) => a
    case a @ Seq(_: AnyFixedLength, _ @_*) => a
    case a @ Seq(_ @_*) =>
      Seq[MP](BackTrackGroup(a.toArray[MP]))
  }


  def splitPrefixSuffix(mpSeq: Seq[MP]): (Seq[MP], Seq[MP]) = {
    def lastNonWildCard(i: Int): Int = if (i < 0) i else mpSeq(i) match {
      case Anything => i
      case _: FixedSequencePrefix => i
      case _ => lastNonWildCard(i - 1)
    }

    val splitIndex = lastNonWildCard(mpSeq.length - 1)
    if (splitIndex == mpSeq.length - 1) {
      mpSeq(splitIndex) match {
        case Anything => 
          (mpSeq, Seq())
        case FixedSequencePrefix(seq) => 
          (mpSeq.dropRight(1) :+ Anything, Array(FixedSequence(seq)))
      }
    } else if (splitIndex < 0) {
      (mpSeq, Seq())
    } else {
      mpSeq(splitIndex) match {
        case FixedSequencePrefix(seq) => 
          (mpSeq.take(splitIndex) :+ Anything, FixedSequence(seq) +: mpSeq.drop(splitIndex + 1))
      }
    }
  }


  trait MP {
    def matchNext(in: CharSequence): Int
    def minLength: Int
  }

  /**
   * Can only be the last pattern
   */
  case object Anything extends MP {
    override def matchNext(in: CharSequence) = in.length
    
    override def toString() = "*"

    override def minLength = 0
  }

  /**
   * Can only be after FixedSequence and FixedSequenceSuffix
   */
  case class AnyFixedLength(length: Int) extends MP  {
     override def matchNext(in: CharSequence) = 
       if (length > in.length) -1 else length
       
     override def toString() = "?" * length

     override def minLength = length
  }

  /**
   * Can only be first matcher, or after AnyFixedLength
   */
  case class FixedSequence(seq: CharSequence) extends MP {
    override def matchNext(in: CharSequence) = {
      if (firstStartsWithSecond(in, seq)) seq.length
      else -1 
    }
    
    override def toString() = seq.toString.replaceAll("\\*", "\\*").replaceAll("\\?", "\\?")

    override val minLength = seq.length
  }

  /**
   * Can be anywhere except after Anything
   */
  case class FixedSequencePrefix(seq: CharSequence) extends MP {
    override def matchNext(in: CharSequence) = {
      val seqLength = seq.length
      val maxIndex = in.length - seq.length

      @tailrec      
      def doMatch(startPos: Int): Int = {
        val curIndex = indexOf(in, seq.charAt(0), startPos)
//        println(s"curIndex = $curIndex")
        if (curIndex < 0 || curIndex > maxIndex) -1
        else if (doesMatch(in, curIndex, seq, 0, seq.length)) curIndex + seq.length
        else doMatch(curIndex + 1)
      }
      
      if (maxIndex < 0) -1
      else doMatch(0)
    }
    
    override def toString() = "*" + seq.toString.replaceAll("\\*", "\\*").replaceAll("\\?", "\\?")

    override val minLength = seq.length
  }
  
  /**
   * Either the only matcher, or concludes matcher list.
   */
  case object Empty extends MP {
    override def matchNext(in: CharSequence) = if (in.length == 0) 0 else -1

    override def minLength = 0
  }
 
  case class BackTrackGroup(mpSeq: Seq[MP]) extends MP {
    //require(mpSeq(0).isInstanceOf[FixedSequencePrefix])
    //require(mpSeq exists { _.isInstanceOf[AnyFixedLength] }, mpSeq mkString "")
    //require(!mpSeq.last.isInstanceOf[AnyFixedLength])

    override val minLength = mpSeq.view.map(_.minLength).sum
    val first = mpSeq(0)
    val remaining = (mpSeq drop 1).toArray

    val minRemainingLength = calcMinLength(remaining)

    final override def matchNext(in: CharSequence) = {
      val result = doMatchNext(in, 0)
      assert(result <= in.length, s"result is ${result - in.length} too big input: $in, pattern: ${mpSeq mkString ""}!" )
      result
    }

    @tailrec
    final def doMatchNext(in: CharSequence, skippedChars: Int): Int = {
      val index = first.matchNext(in)
      if (index < 0) -1
      else {
        val matches = matchRemaining(in.subSequence(index, in.length))
        if (matches) {
          minRemainingLength + index + skippedChars
        } else {
          doMatchNext(in.subSequence(1, in.length), skippedChars + 1)
        }
      }

    }

    def matchRemaining(in: CharSequence): Boolean = {
      def matchRemaining0(in: CharSequence, curMatcherIndex: Int): Boolean = {
        if (curMatcherIndex >= remaining.length) true
        else {
          val resultPos = remaining(curMatcherIndex).matchNext(in)
          if (resultPos < 0) false
          else matchRemaining0(in.subSequence(resultPos, in.length), curMatcherIndex + 1)
        }
      }

      matchRemaining0(in, 0) 
    }

    override def toString = mpSeq mkString ""
  }
  
  
  def indexOf(seq: CharSequence, ch: Char): Int = indexOf(seq, ch, 0)
  
  @tailrec 
  def indexOf(seq: CharSequence, ch: Char, index: Int): Int = 
    if (index >= seq.length) -1
    else if (seq.charAt(index) == ch) index
    else indexOf(seq, ch, index + 1)
    
  def doesMatch(seq1: CharSequence, index1: Int, seq2: CharSequence, index2: Int, count: Int): Boolean = {
     if (seq1.length - index1 < count || seq2.length - index2 < count) {
       false
     } else {
        val maxI1 = index1 + count
        
        def matches(i1: Int, i2: Int): Boolean = {
          if (i1 == maxI1) true
          else if (seq1.charAt(i1) == seq2.charAt(i2)) matches(i1 + 1, i2 + 1)
          else false
        }
        
        matches(index1, index2)
     }
  }

  def firstStartsWithSecond(seq: CharSequence, prefix: CharSequence): Boolean = {
    val maxIndex = prefix.length
    if (seq.length < maxIndex) {
      false
    } else {
      @tailrec
      def mt(i: Int): Boolean = 
        if (i == maxIndex) true 
        else if (seq.charAt(i) == prefix.charAt(i)) mt(i + 1) 
        else false

      mt(0)
    }
  }

  def parseMP(pattern: String): Seq[MP] = {
    if (pattern == "") Array(Empty)
    else new MPParser(pattern).parse()
  }

  def calcMinLength(seq: Array[MP]): Int = {
    def cc(index: Int, length: Int): Int = {
      if (index < 0) length
      else cc(index - 1, length + seq(index).minLength)
    }

    cc(seq.length - 1, 0)
  }

}

import GlobMatcher._
class GlobMatcher(pMpSeq: Array[MP], pPrefixSeq: Array[MP], pSuffixSeq: Array[MP], val pattern: String) {
  
  private[this] val mpSeq: Array[MP] = pMpSeq
  private[this] val prefixSeq = pPrefixSeq
  private[this] val suffixSeq = pSuffixSeq
  private[this] val minLength = calcMinLength(mpSeq)
  private[this] val suffixLength = calcMinLength(suffixSeq)

    
  def matches(seq: CharSequence): Boolean = {
    if (seq.length < minLength) false
    else if (suffixSeq.length > 0) {
      val suffix = seq.subSequence(seq.length - suffixLength, seq.length)
      matches(suffixSeq, suffix, 0, 0) && matches(prefixSeq, seq, 0,0)
    } else {
      matches(prefixSeq, seq, 0, 0)
    }
  }

  def matcherTree = Seq(mpSeq: _*)
  
  @tailrec
  final def matches(mpSeq: Array[MP], seq: CharSequence, seqOffset: Int, mpOffset: Int): Boolean = {
    if (mpOffset >= mpSeq.length) seqOffset == seq.length
    else {
      val seqPart = seq.subSequence(seqOffset, seq.length)
      val subResult = mpSeq(mpOffset).matchNext(seqPart)
      if (subResult < 0) false
      else {
        val newOffset = seqOffset + subResult
        matches(mpSeq, seq, newOffset, mpOffset + 1)
      }
    }
  }
}



