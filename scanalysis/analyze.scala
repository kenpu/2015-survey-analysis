import scala.io.Source
import scala.collection.immutable.TreeMap


object Analyze {
    type Survey = Map[String, String]
    def parse(line: String): Survey = {
        val fields = line split "\t"

        val course = fields(1) match {
        case x if x contains "CSCI 4020" => "COMPILER"
        case x if x contains "CSCI 2020" => "SYSDEV"
        case x if x contains "CSCI 3055" => "PL"
        case _ => "UNK"
        }

        val social = fields(2) match {
        case x if x contains "blackboard" => "BB"
        case _ => "G+"
        }

        val content = fields(4) match {
        case x if(x contains "powerpoint") => "PP"
        case _ => "HTML"
        }

        Map("course" -> course, 
               "social" -> social, 
               "content" -> content)
    }

    def loadResults(filename: String): List[Survey] = {
        val lines = Source.fromFile(filename).getLines().toList
        for(line <- lines.tail) yield parse(line)
    }

    def main(args: Array[String]) = {
        val results = loadResults(args(0))
        println("---------------------------------")
        println("total: " + results length)
        println("---------------------------------")
        println("SOCIAL")
        Query.print(Query.groupby(results, "social"))
        println("---------------------------------")
        println("CONTENT")
        Query.print(Query.groupby(results, "content"))
    }
}

object Query {
    def print(grouping: Map[String, Int]) {
        for ((k,v) <- grouping) {
            println("%10s   %d".format(k,v))
        }
    }
    def groupby(results: List[Analyze.Survey], keys: String*) = {
        val f = {x: Analyze.Survey => (for(k <- keys) yield x(k)).mkString}
        TreeMap(results.groupBy(f).toList.map({
            x => (x._1, x._2.length)
        }): _*)
    }

}
