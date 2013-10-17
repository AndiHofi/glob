import scala.annotation.tailrec

object GlobMatcher {
  trait MP {
    def matchNext(in: CharSequence): Int
  }

  class Anything extends MP {
    override def matchNext(in: CharSequence) = in.length
    
    override def toString() = "*"
  }

  class AnyFixedLength(length: Int) extends MP  {
     override def matchNext(in: CharSequence) = 
       if (length > in.length) -1 else length
       
     override def toString() = "*" * length
  }

  class FixedSequence(seq: CharSequence) extends MP {
    override def matchNext(in: CharSequence) = {
      if (firstStartsWithSecond(in, seq)) seq.length
      else -1 
    }
    
    override def toString() = seq.toString.replaceAll("\\*", "\\*").replaceAll("\\?", "\\?")
  }

  class FixedSequencePrefix(seq: CharSequence) extends MP {
    override def matchNext(in: CharSequence) = {
      val seqLength = seq.length
      val maxIndex = in.length - seq.length

      @tailrec      
      def doMatch(startPos: Int): Int = {
        val curIndex = indexOf(in, seq.charAt(0), startPos)
        if (curIndex < 1 || curIndex > maxIndex) -1
        else if (doesMatch(in, curIndex, seq, 0, seq.length)) curIndex + seq.length
        else doMatch(curIndex + 1)
      }
      
      if (0 > maxIndex) -1
      else doMatch(0)
    }
    
    override def toString() = "*" + seq.toString.replaceAll("\\*", "\\*").replaceAll("\\?", "\\?")
  }
  
  class Empty extends MP {
    override def matchNext(in: CharSequence) = if (in.length == 0) 0 else -1
  }
  
  
  
  def indexOf(seq: CharSequence, ch: Char): Int = indexOf(seq, ch, 0)
  
  def indexOf(seq: CharSequence, ch: Char, index: Int): Int = 
    if (index >= seq.length) -1
    else if (seq.charAt(index) == ch) index
    else indexOf(seq, ch, index + 1)
    
  def doesMatch(seq1: CharSequence, index1: Int, seq2: CharSequence, index2: Int, count: Int): Boolean = {
     if (seq1.length - index1 < count || seq2.length -index2 < count) false
     else {
        val maxI1 = index1 + count
        
        def matches(i1: Int, i2: Int): Boolean = {
          if (i1 == count) true
          else if (seq1.charAt(i1) == seq2.charAt(i2)) matches(i1 +1, i2+1)
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
  
  def parseMP(pattern: String): Array[MP] = {
    if (pattern == "") Array(new Empty)
    else new MPParser(pattern).parse().toArray
  }
}

class GlobMatcher(val pattern: String) {
  import GlobMatcher._
  
  private[this] val mpSeq: Array[MP] = GlobMatcher.parseMP(pattern)
  
  def matches(seq: CharSequence): Boolean = {
    matches(seq, 0, 0)
  }
  
  @tailrec
  final def matches(seq: CharSequence, seqOffset: Int, mpOffset: Int): Boolean = {
    if (mpOffset >= mpSeq.length) seqOffset == seq.length
    else {
      val seqPart = seq.subSequence(seqOffset, seq.length)
      val subResult = mpSeq(mpOffset).matchNext(seqPart)
      if (subResult < 0) false
      else {
        val newOffset = seqOffset + subResult
        matches(seq, newOffset, mpOffset + 1)
      }
    }
  }
}


class MPParser(input: String) {
  import GlobMatcher._
  
  val scanner = new MPScanner(input)
  
  def parse(): Seq[MP] = {
    val buf = scala.collection.mutable.ArrayBuffer[MP]()
    
    def parseNext(): Seq[MP] = {
      scanner.nextToken() match {
        case e: Empty => 
          if (buf.isEmpty) buf += e else ()
          buf
        case tk =>
          buf += tk
          parseNext()
      }
    }
    
    parseNext()
  }
  
  
}

class MPScanner(input: String) {
  import GlobMatcher._
  
  final val EOF = '\u0000'

  var pendingWildcard = false
  var curPos = -1
  var ch: Char = _
  var nCh: Char = _
  
  nextCh()
  nextCh()
  
  def nextCh(): Char = {
    ch = nCh
    val nextPos = curPos + 1
    if (nextPos >= input.length) {
      nCh = EOF
    } else {
      nCh = input.charAt(nextPos)
      curPos = nextPos
    }
    
    ch
  }

  def nextToken(): MP = {
    ch match {
      case EOF => 
        if (pendingWildcard) new GlobMatcher.Anything()
        else new GlobMatcher.Empty()
      case '?' | '*' => 
        parseWildcard(0)
      case _ =>
        parseFixedString()
    }
  }
  
  def parseWildcard(minLength: Int): MP = {
    if (ch == '*') {
      pendingWildcard = true
      nextCh()
      parseWildcard(minLength)
    } else if (ch == '?') {
      nextCh()
      parseWildcard(minLength + 1)
    } else if (minLength > 0) {
      new GlobMatcher.AnyFixedLength(minLength)
    } else if (ch == EOF) {
      if (pendingWildcard) new GlobMatcher.Anything()
      else throw new IllegalStateException()
    } else {
      parseFixedString()
    }
  }
  
  def parseFixedString(): MP = {
    val sb = new StringBuilder()
    
    @tailrec
    def nn(): MP = {
      ch match {
        case EOF | '*' | '?' => 
          if (pendingWildcard) new GlobMatcher.FixedSequencePrefix(sb.toString)
          else new GlobMatcher.FixedSequence(sb.toString)
        case '\\' =>
          nextCh()
          ch match {
            case '*' | '?' | '\\'=> 
              sb.append(ch)
              nextCh()
              nn()
            case _ => throw new RuntimeException()
          }
        case _ =>
          sb.append(ch)
          nextCh()
          nn()
      }
    }
    
    nn()
  }
}
