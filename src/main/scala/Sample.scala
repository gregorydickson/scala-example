import scala.io._
import scala.collection.mutable.Map
import scala.collection.immutable.ListMap 


object Sample extends App {
  	val file = "./src/main/resources/DoI.txt"
  
	val src = Source.fromFile(file)
	val fileStats = new FileStats(src)   
	println("Stats for file: "+ file)

	println("Line Count:" + fileStats.lineCount)

	println("Total Word Count: "+fileStats.totalWordCount) 
	val testWord = "People"
	var inputMap = scala.collection.mutable.Map[String, Int]()
    val frequencyMap = fileStats.wordFreq(inputMap)
	println("Count for " + testWord + ": " +frequencyMap(testWord.toLowerCase))
	
	val sortedListMap:ListMap[String,Int] =  ListMap(frequencyMap.toList.sortBy{-_._2}:_*)

	def report(freq: ListMap[String, Int], count: Int, top: List[String]): List[String] = {
  		if (count == 3){
  		 	return top
    	} else {
    		var max = freq.head
    		var newCount = count
    		newCount += 1
    		val newList = max._1 :: top
    		report(freq.tail,newCount,newList )
    	}
  	}
  	val topThree = report(sortedListMap, 0, List[String]())
  	println("Top Three Words by Freq: " + topThree(0) + ", " + topThree(1) + ", " + topThree(2))
}

class FileStats(source: BufferedSource) {
  
  
  val list = source.getLines.toList.map(line => line.split(" |--").toList)
  val flattenedList = list.flatten.filter(_.nonEmpty).filter(_.matches("[&a-zA-Z,.:';-]+"))
  
  def lineCount: Int = list.size 

  
  def totalWordCount: Int = flattenedList.size

  

  def countForWord(word: String): Int = {
  	val lowerWord = word.toLowerCase
  	val wordList = flattenedList.map(_.toLowerCase).filter(_ contains lowerWord)
  	wordList.size
  }

  def wordFreq(freq: Map[String, Int]): Map[String, Int] = {
  	val lowerNoPunctuationList = flattenedList.map(_.replaceAll("[,.:';]+","").toLowerCase)
  	
  	def frequency(freq: Map[String, Int], lowerNoPunctuationList: List[String]): Map[String, Int] = {
  		if (lowerNoPunctuationList == Nil){
  		 	freq
    	} else {
    		var word = lowerNoPunctuationList.head
    		if(freq.contains(word)) freq(word) += 1
    		else freq(word) = 1
    		frequency(freq, lowerNoPunctuationList.tail)
    	}
  	}
  	frequency(freq, lowerNoPunctuationList)

  }





}
