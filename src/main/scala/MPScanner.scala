import scala.annotation.tailrec
import GlobMatcher._

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
    //println(s"current char is $ch")
    ch
  }

  def nextToken(): MP = {
    //println(s"Called nextToken() ch=$ch")

    ch match {
      case EOF => 
        if (pendingWildcard) {
          pendingWildcard = false
          GlobMatcher.Anything
        }
        else GlobMatcher.Empty
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
      if (pendingWildcard) {
        pendingWildcard = false
        GlobMatcher.Anything
      } else {
        throw new IllegalStateException()
      }
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
          if (pendingWildcard) {
            pendingWildcard = false
            new GlobMatcher.FixedSequencePrefix(sb.toString)
          } else {
            new GlobMatcher.FixedSequence(sb.toString)
          }
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
