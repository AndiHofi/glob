import scala.annotation.tailrec
import GlobMatcher._

class MPParser(input: String) {
  
  val scanner = new MPScanner(input)
  
  def parse(): Seq[MP] = {
    val buf = scala.collection.mutable.ArrayBuffer[MP]()
    
    @tailrec
    def parseNext(): Seq[MP] = {
      scanner.nextToken() match {
        case Empty => 
          if (buf.isEmpty) buf += Empty else ()
          buf
        case tk =>
          // println(s"Found token $tk")
          buf += tk
          parseNext()
      }
    }
    
    val result = parseNext()
    //println(s"Parsing done: $result")
    result
  }
  
  
}


