// Import necessary libraries
import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

object Main {
  def main(args: Array[String]): Unit = {

    // *******************************************************************************************************************
    // MAIN APPLICATION LOGIC
    // *******************************************************************************************************************

    // Read data from file
    val mapData: Map[String, Int] = readFile("./src/data.txt")

    // Display loaded data for verification
    println(mapData)

    // Define new menu options as a Map of actions
    val actionMap: Map[Int, () => Boolean] = Map(
      1 ->
      2 ->
      3 ->
    )


    // Application menu loop
    var opt: Int = 0
    do {
      opt = displayMenuAndReadOption() // Show menu and get user input
    }
    while (processMenuOption(opt)) // Process user choice until quit option


    // *******************************************************************************************************************
    // MENU HANDLING FUNCTIONS
    // *******************************************************************************************************************


    // Displays the menu and reads user input
    // returns the menu option selected by the user
    def displayMenuAndReadOption(): Int = {
      println(
        """|Please select one of the following:
           |  1 - Show points for all teams
           |  2 - Show points for chosen team
           |  3 - Quit""".stripMargin)
      readInt()
    }


    def processMenuOption(option: Int): Boolean = {
      actionMap.get(option) match {
        case Some(action) => action() // Invoke the corresponding action
        case None =>
          println("Invalid option. Please try again.")
          true
      }
    }

    // *******************************************************************************************************************
    // UTILITY FUNCTIONS
    // *******************************************************************************************************************

    def readFile(filename: String): Map[String, Int] = {
      var mapBuffer: Map[String, Int] = Map()

      try {
        for (line <- Source.fromFile(filename).getLines()) {
          val splitline = line.split(",").map(_.trim).toList
          mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.head.toInt)
        }
      } catch {
        case ex: Exception => println(s"Error reading file: ${ex.getMessage}")
      }

      mapBuffer
    }

  }
}

