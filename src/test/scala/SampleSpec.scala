

import java.io.File
import org.scalatest._
import scala.io._
import scala.collection.mutable.Map

class SampleSpec extends FlatSpec with Matchers {

  behavior of "FileStats"

  val src = Source.fromFile("./src/test/resources/DoI.txt")
  val fileStats = new FileStats(src)

  

  it must "count the lines in a file" in {
    val expectedLines = 41
    fileStats.lineCount should be (expectedLines)
  }
  it must "count the words in a file" in {
    val expectedWords = 1335
    fileStats.totalWordCount should be (expectedWords)
  }

  it must "count occurences of a specific word in a file" in {
    val peopleCount = 10
    fileStats.countForWord("People") should be (peopleCount)
  }

  it must "calculate the frequency of all words in a file" in {
    var frequencyMap = scala.collection.mutable.Map[String, Int]()
    val returnMap = fileStats.wordFreq(frequencyMap)
    returnMap("the") should be (78)

  }
  

}
